// ex: ts=2 sw=2 ft=cpp et indentexpr=
/**
 * \file
 * \brief Fixed size blocks storage - non-atomic version
 * \author Dmitriy Kargapolov
 * \since 30 June 2024
 */
#pragma once

#ifndef __has_builtin
  #error "__has_builtin is not defined"
#endif

#if not __has_builtin(__builtin_ffsll)
  #error "__builtin_ffsll is not defined"
#endif

#ifndef BS_LEVELS
#define BS_LEVELS 3
#endif

namespace {

template<int N>
inline size_t block_size(size_t bs, int n = 64) {
  return 8 + n * block_size<N-1>(bs);
}

template<>
inline size_t block_size<0>(size_t bs, int) { return bs; }

template<int N>
inline char *ptr(void *mem, int n, size_t bs) {
  return (char *)mem + block_size<N>(bs, n);
}

template<int N>
inline void *pointer(void *mem, int addr, size_t bs) {
  return pointer<N-1>(ptr<N>(mem, addr % 64, bs), addr / 64, bs);
}

template<>
inline void *pointer<0>(void *mem, int, size_t) { return mem; }

template<int N>
int alloc(void *mem, void *end, size_t bs) {
  uint64_t& mask = *(uint64_t *)mem;

  while (true) {
    // find group that not yet filled (marked by 1)
    int n = __builtin_ffsll(mask) - 1;
    if (n < 0)
      return -1;

    // pointer to the group mask
    char *p = ptr<N>(mem, n, bs);
    if (p + 8 > end) return -2;

    // find subgroup or free block
    int m = alloc<N-1>(p, end, bs);
    if (m < -1) return m;

    if (m < 0 || m == 63) {
      // ensure mask bit cleared to mark group filled
      mask &= ~(1 << n);

      // try another group
      if (m < 0) continue;
    }

    // return index
    return m * 64 + n;
  }
}

template<>
int alloc<1>(void *mem, void *end, size_t bs) {
  uint64_t& mask = *(uint64_t *)mem;

  int n = __builtin_ffsll(mask) - 1;
  if (n < 0)
    return -1;

  char *p = (char *)mem + n * bs;
  if (p + bs > end) return -2;

  mask ^= (1 << n);
  return n;
}

template<int N>
int store_(void *mem, void *end, const ErlNifBinary& bin, size_t bs) {
  int n = alloc<N>(mem, end, bs);
  if (n < 0) return n;
  memcpy(pointer<N>(mem, n, bs), bin.data, bin.size);
  return n;
}

}

struct bs_head {
  uint32_t block_size;
  uint64_t mask;

  void init(unsigned block_size_) {
    block_size = block_size_;
    mask = 0xffff'ffff'ffff'ffff;
  }

  int store(void *mem, void *end, const ErlNifBinary& bin) {
    return store_<BS_LEVELS>(mem, end, bin, block_size);
  }
};

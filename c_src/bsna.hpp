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
#define BS_LEVELS 2
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

static inline
uint64_t& get_mask(void* mem, void*& last) {
  // initialize mask if not yet done
  if (mem > last) {
    *(uint64_t*)mem = 0xffff'ffff'ffff'ffff;
    last = mem;
  }
  return *(uint64_t *)mem;
}

template<int N>
int alloc(void *mem, void*& last, void *end, size_t bs) {
  if ((char *)mem + 8 > end) return -2;
  uint64_t& mask = get_mask(mem, last);

  while (true) {
    // find group that not yet filled (marked by 1)
    int n = __builtin_ffsll(mask) - 1;
    if (n < 0) return -1;

    // pointer to the group mask
    char *p = ptr<N>(mem, n, bs);

    // find subgroup or free block
    int m = alloc<N-1>(p, last, end, bs);
    if (m < -1) return m;

    if (m < 0) {
      // ensure mask bit cleared to mark group filled
      mask &= ~(1ul << n);

      // try another group
      continue;
    }

    // return index
    return m * 64 + n;
  }
}

template<>
int alloc<1>(void *mem, void*& last, void *end, size_t bs) {
  if ((char *)mem + 8 > end) return -2;
  uint64_t& mask = get_mask(mem, last);

  int n = __builtin_ffsll(mask) - 1;
  if (n < 0) return -1;

  // pointer to the data block
  char *p = ptr<1>(mem, n, bs);
  if (p + bs > end) return -2;

  mask ^= (1ul << n);
  return n;
}

template<int N>
int store(void *mem, void*& last, void *end, const ErlNifBinary& bin, size_t bs) {
  int n = alloc<N>(mem, last, end, bs);
  if (n < 0) return n;
  memcpy(pointer<N>(mem, n, bs), bin.data, bin.size);
  return n;
}

}

struct bs_head {
  uint32_t block_size;
  ssize_t limo; // Last Initialized Mask Offset

  void init(unsigned block_size_) {
    block_size = block_size_;
    limo = -1;
  }

  int store(void *mem, void *end, const ErlNifBinary& bin) {
    void *last = (char *)mem + limo;
    int ret = ::store<BS_LEVELS>(mem, last, end, bin, block_size);
    limo = (char *)last - (char *)mem;
    return ret;
  }
};

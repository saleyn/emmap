// ex: ts=2 sw=2 ft=cpp et
/**
 * \file
 * \brief Fixed size blocks storage
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

struct bs_head {
  size_t block_size;
};

namespace fsbs {

template<int N>
size_t block_size(size_t bs, int n = 64) { return 8 + n * block_size<N-1>(bs); }

template<>
size_t block_size<0>(size_t bs, int) { return bs; }

template<int N>
int alloc(void* mem, size_t bs) {
  auto mask_ptr = static_cast<std::atomic<uint64_t> *>(mem);

  while (true) {
    // find group that not yet filled (marked by 1)
    uint64_t mask = mask_ptr->load(std::memory_order_relaxed);
    int n = __builtin_ffsll(mask) - 1;
    if (n < 0)
      return -1;

    // pointer to the group mask
    char *p = (char *)mem + block_size<N>(bs, n);

    // find subgroup or free block
    int m = alloc<N-1>(p, bs);
    if (m < 0 || m == 63) {
      // ensure mask bit cleared to mark group filled
      uint64_t new_mask;
      do
        new_mask = mask & ~(1 << n);
      while (!mask_ptr->compare_exchange_weak(mask, new_mask,
        std::memory_order_release, std::memory_order_relaxed));

      // try another group
      if (m < 0) continue;
    }

    // return index
    return m * 64 + n;
  }
}

template<>
int alloc<0>(void* mem, size_t) {
  auto mask_ptr = static_cast<std::atomic<uint64_t> *>(mem);
  uint64_t mask = mask_ptr->load(std::memory_order_relaxed);

  while (true) {
    int n = __builtin_ffsll(mask) - 1;
    if (n < 0)
      return -1;

    uint64_t new_mask = mask ^ (1 << n);
    if (mask_ptr->compare_exchange_weak(mask, new_mask,
        std::memory_order_release, std::memory_order_relaxed))
      return n;
  }
}

int alloc(void* mem, size_t bs) {
  return alloc<2>(mem, bs);
}

}

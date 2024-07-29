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

const uint64_t filled = 0xffff'ffff'ffff'ffff;
const uint64_t vacant = 0x0000'0000'0000'0000;

static inline uint64_t& free_mask(void *mem) { return ((uint64_t*)mem)[0]; }
static inline uint64_t& used_mask(void *mem) { return ((uint64_t*)mem)[1]; }

template<int N>
inline bool not_used(void *mem) { return used_mask(mem) == vacant; }
template<>
inline bool not_used<1>(void *mem) { return free_mask(mem) == filled; }

static inline void maybe_init_mask(void* mem, void*& last) {
  // initialize ground level mask if not yet done
  if (mem > last) {
    free_mask(mem) = filled;
    last = mem;
  }
}

static inline void maybe_init_masks(void* mem, void*& last) {
  // initialize masks if not yet done
  if (mem > last) {
    free_mask(mem) = filled;
    used_mask(mem) = vacant;
    last = mem;
  }
}

template<int> inline size_t mask_len() { return 16; }
template<> inline size_t mask_len<1>() { return 8; }

// calculate block size at given level
// all levels excepting ground carry two 8-byte masks:
// free blocks and allocated blocks masks
template<int N>
inline size_t block_size(size_t bs, int n = 64) {
  return mask_len<N>() + n * block_size<N-1>(bs);
}

// ground level carry onle "available" mask
template<>
inline size_t block_size<1>(size_t bs, int n) {
  return mask_len<1>() + n * bs;
}

// translate address to pointer
template<int N>
inline char *ptr(void *mem, int n, size_t bs) {
  return (char *)mem + block_size<N>(bs, n);
}

// translate address to a pointer
template<int N>
inline void *pointer(void *mem, int addr, size_t bs) {
  return pointer<N-1>(ptr<N>(mem, addr % 64, bs), addr / 64, bs);
}

template<>
inline void *pointer<0>(void *mem, int, size_t) { return mem; }

struct limits {
  size_t bs;
  void *end;
  void *last;

  limits(size_t b, void *e, void *l) : bs(b), end(e), last(l) {}

  template<int N>
  bool mask_over(const void *ptr) const {
    return (char *)ptr + mask_len<N>() > end;
  }

  template<int N>
  bool mask_undef(const void *ptr) const {
    return ptr > last || (char *)ptr + mask_len<N>() > end;
  }

  bool data_over(const void *ptr) const {
    return (char *)ptr + bs > end;
  }
};

// translate address to pointer to an existing block
template<int N>
inline void *real_pointer(void *mem, int addr, const limits& lim) {
  if (lim.mask_undef<N>(mem)) return nullptr;
  void *p = ptr<N>(mem, addr % 64, lim.bs);
  return real_pointer<N-1>(p, addr / 64, lim);
}

template<>
inline void *real_pointer<1>(void *mem, int addr, const limits& lim) {
  if (lim.mask_undef<1>(mem)) return nullptr;
  int n = addr % 64;
  // attempt to access unallocated block?
  if ((free_mask(mem) & (1ul << n)) != vacant) return nullptr;
  void *p = ptr<1>(mem, n, lim.bs);
  return lim.data_over(p) ? nullptr : p;
}

// free block
template<int N>
inline bool free_block(void *mem, int addr, const limits& lim) {
  // bit
  int n = addr % 64;
  uint64_t bit = 1ul << n;
  bool ret;

  // next level pointer
  void *p = ptr<N>(mem, n, lim.bs);
  if (lim.mask_undef<N-1>(p)) {
    ret = false;

    // clear presence mask
    used_mask(mem) &= ~bit;
  }
  else {
    ret = free_block<N-1>(p, addr / 64, lim);

    // clear presence mask if next level got empty
    if (not_used<N-1>(p)) used_mask(mem) &= ~bit;
  }

  // mark block free
  free_mask(mem) |= bit;

  return ret;
}

// free block
template<>
inline bool free_block<1>(void *mem, int addr, const limits&) {
  // bit
  int n = addr % 64;
  uint64_t bit = 1ul << n;

  // attempt to free unallocated block?
  if ((free_mask(mem) & bit) != vacant) return false;

  // mark block free
  free_mask(mem) |= bit;

  return true;
}

template<int N>
int alloc(void *mem, limits& lim) {
  if (lim.mask_over<N>(mem)) return -2;
  maybe_init_masks(mem, lim.last);
  uint64_t& free_bits = free_mask(mem);

  while (true) {
    // find group that not yet filled (marked by 1)
    int n = __builtin_ffsll(free_bits) - 1;
    if (n < 0) return -1;

    // pointer to the group mask
    char *p = ptr<N>(mem, n, lim.bs);

    // find subgroup or free block
    int m = alloc<N-1>(p, lim);
    if (m < -1) return m;

    // n-th bit
    uint64_t bit = 1ul << n;

    if (m < 0) {
      // ensure free mask bit cleared to mark group filled
      free_bits ^= bit;

      // try another group
      continue;
    }

    // if group is filled, clear free mask bit
    if (*(uint64_t *)p == 0)
      free_bits ^= bit;

    // ensure used mask bit is set
    used_mask(mem) |= bit;

    // return index
    return m * 64 + n;
  }
}

template<>
int alloc<1>(void *mem, limits& lim) {
  if (lim.mask_over<1>(mem)) return -2;
  maybe_init_mask(mem, lim.last);
  uint64_t& free_bits = free_mask(mem);

  // find block that not yet filled (marked by 1)
  int n = __builtin_ffsll(free_bits) - 1;
  if (n < 0) return -1;

  // pointer to the data block
  char *p = ptr<1>(mem, n, lim.bs);
  if (lim.data_over(p)) return -2;

  // flip 1-bit to 0 marking block allocated
  free_bits ^= 1ul << n;

  return n;
}

template<int N>
int store(void *mem, const ErlNifBinary& bin, limits& lim) {
  int n = alloc<N>(mem, lim);
  if (n < 0) return n;
  memcpy(pointer<N>(mem, n, lim.bs), bin.data, bin.size);
  return n;
}

// fold through stored blocks

template<int N, typename F> struct proc {
static void fold(void *mem, const limits& lim, F fun) {
  uint64_t bit = 1;
  for (int n = 0; n < 64; ++n, bit <<= 1) {
    if ((used_mask(mem) & bit) != bit)
      continue;
    void *p = ptr<N>(mem, n, lim.bs);
    if (lim.mask_undef<N-1>(p))
      break;
    proc<N-1, F>::fold(p, lim, fun);
  }
} };

template<typename F> struct proc<1, F> {
static void fold(void *mem, const limits& lim, F fun) {
  uint64_t bit = 1;
  for (int n = 0; n < 64; ++n, bit <<= 1) {
    if ((free_mask(mem) & bit) == bit)
      continue;
    void *p = ptr<1>(mem, n, lim.bs);
    if (lim.data_over(p))
      break;
    fun(p, lim.bs);
  }
} };

}

struct bs_head {
  uint32_t block_size;
  ssize_t limo; // Last Initialized Mask Offset

  void init(unsigned block_size_) {
    block_size = block_size_;
    limo = -1;
  }

  int store(void *mem, void *end, const ErlNifBinary& bin) {
    limits lim(block_size, end, (char *)mem + limo);
    int ret = ::store<BS_LEVELS>(mem, bin, lim);
    limo = (char *)lim.last - (char *)mem;
    return ret;
  }

  template<typename T>
  bool read(void *mem, void *end, int addr, T consume) {
    limits lim(block_size, end, (char *)mem + limo);
    void *ptr = ::real_pointer<BS_LEVELS>(mem, addr, lim);
    if (ptr) consume(ptr, block_size);
    return ptr;
  }

  bool free(void *mem, void *end, int addr) {
    limits lim(block_size, end, (char *)mem + limo);
    if (lim.mask_undef<BS_LEVELS>(mem)) return false;
    return ::free_block<BS_LEVELS>(mem, addr, lim);
  }

  template<typename F>
  void fold(void *mem, void *end, F fun) {
    limits lim(block_size, end, (char *)mem + limo);
    if (lim.mask_undef<BS_LEVELS>(mem)) return;
    proc<BS_LEVELS, F>::fold(mem, lim, fun);
  }
};

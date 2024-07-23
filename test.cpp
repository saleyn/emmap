
#include <stdlib.h>
#include <assert.h>

template<int N>
size_t block_size(size_t bs, int n = 64) { return 8 + n * block_size<N-1>(bs); }

template<>
size_t block_size<0>(size_t bs, int) { return bs; }

template<int N>
void *addr(void *mem, int n, size_t bs) {
  return addr<N-1>((char *)mem + block_size<N>(bs, n % 64), n / 64, bs);
}

template<>
void *addr<0>(void *mem, int, size_t) { return mem; }

int main() {

  assert(10 == block_size<0>(10, 3));
  assert(8 + 3 * 10 == block_size<1>(10, 3));
  assert(8 + 15 * (8 + 64 * 10) == block_size<2>(10, 15));

  static char buff[256];
  void *p = buff;

  assert(p == addr<0>(p, 1, 10));
  assert((char *)p
    + 8
    + 13 * 10
    == addr<1>(p, 13, 10));

  assert((char *)p
    + 8
    + 7 * (8 + 64 * 11)
    + 8
    + 13 * 11
    == addr<2>(p, (13 * 64 + 7), 11));

  return 0;
}

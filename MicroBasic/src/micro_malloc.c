#include "micro_malloc.h"

#define MEMORY_SIZE 0x7500
#define N_BLOCKS (MEMORY_SIZE / MALLOC_BLOCK_SIZE)

byte memory[MEMORY_SIZE];
byte availability[N_BLOCKS];


int find_n_free(int n) {
  int i, j;
  for (i = 0; i < N_BLOCKS; i++) {
    if (!availability[i]) {
      for (j = i; i < j + n && i < N_BLOCKS; i++) {
        if (availability[i]) break;
      }
      if (i != j + n) continue;
      return j;
    }
  }
  return -1;
}

void mmset(void* ptr, byte to, int n) {
  int i;
  for (i = 0; i < n; i++) {
    ((byte*) ptr)[i] = to;
  }
}


void* mmalloc(int size) {
  int n, x, i;
  n = ((size - 1) / MALLOC_BLOCK_SIZE) + 1;
  x = find_n_free(n);
  for (i = x; i < x + n; i++) {
    availability[i] = n - i - x;
  }
  return memory + x;
}

int ptr_diff(void* ptr1, void* ptr2) {
  return (byte*)ptr1 - (byte*)ptr2;
}

void mmcpy(void* ptr1, int size, void* ptr2) {
  int i;
  for (i = 0; i < size; i++) {
    ((byte*)ptr2)[i] = ((byte*)ptr1)[i];
  }
}

void mfree(void* ptr) {
  int n, i, size;
  n = ptr_diff(ptr, memory);
  size = availability[n];
  for (i = 0; i < size; i++) {
    availability[n+i] = 0;
  }
}

void* mrealloc(void* ptr, int size) {
  int n, c_size;
  void* newPtr;
  n = ptr_diff(ptr, memory);
  c_size = availability[n];
  if (size / MALLOC_BLOCK_SIZE == c_size / MALLOC_BLOCK_SIZE)
    newPtr= ptr;
  else {
    newPtr = mmalloc(size);
    mmcpy((byte*)ptr, c_size, (byte*)newPtr);
    mfree(ptr);
  }
  return newPtr;
}



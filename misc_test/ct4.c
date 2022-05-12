#include <stdio.h>
#include <stdlib.h>


int main() {
  struct {struct {unsigned char* ptr; unsigned int len;}* ptr; unsigned int len;} args;
  void* x = (void*)&args;
  printf("size: %d vs %d\n", sizeof(&args), sizeof(x));
}
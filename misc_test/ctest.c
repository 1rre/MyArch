#include <stdio.h>

unsigned char return_six() {
  return 6;
}

unsigned char return_seven() {
  return 7;
}

void* rv_fn_1_a2_c1(void* _args) {
  struct args_t {
    unsigned char _1;
    unsigned char (*_2)();
  } x = *(struct args_t*)_args;
  printf("Calling %p\n", x._2);
  return (void*)(long)x._2();
}

void* rv_fn_manage_1(int n_args, void* args[n_args]) {
  switch (n_args) {
    case 2:
      struct fnc_0_args {
        int _1;
        unsigned char (*_2)();
      } *fn_args = (struct fnc_0_args*)args;
      if (fn_args->_1) return rv_fn_1_a2_c1(args);
  }
}

int main() {
  struct {
    int _1;
    unsigned char (*_2)();
  } fnc_1_args;
  fnc_1_args._1 = 0;
  fnc_1_args._2 = return_six;
  unsigned char rtnval = (unsigned char)(long)rv_fn_manage_1(2, (void*)&fnc_1_args);
  printf("%d\n", rtnval);
  return 0;
}

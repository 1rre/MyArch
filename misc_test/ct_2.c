#include <stdio.h>
#include <stdlib.h>

void* rv_fn_manage_println(int n_args, void* args) {
  switch (n_args) {
    case 1:
      struct rv_fnm_args_t0 {
        struct {unsigned char* ptr; unsigned int len;} s; // _0
      } *rv_fnm_args0 = (struct rv_fnm_args_t0*)args;
      struct rv_fnm_args_t1 {
        signed int i; // _0
      } *rv_fnm_args1 = (struct rv_fnm_args_t1*)args;
    break;
  }
}
void* rv_fn_manage_main(int n_args, void* args) {
  switch (n_args) {
    case 1:
      struct rv_fnm_args_t {
        struct {struct {unsigned char* ptr; unsigned int len;}* ptr; unsigned int len;} args; // _0
      } *rv_fnm_args = (struct rv_fnm_args_t*)args;
    printf("Len: %d\n", rv_fnm_args->args.len);
    return 0;
    break;
  }
}

void* println(unsigned int n_args, void* args) {return rv_fn_manage_println(n_args, args);}
void* rv_main(unsigned int n_args, void* args) {return rv_fn_manage_main(n_args, args);}

int main(int n_args, char** _args) {
  struct {struct {unsigned char* ptr; unsigned int len;}* ptr; unsigned int len;} args;
  args.len = n_args;
  args.ptr = malloc(n_args * sizeof(struct {unsigned char* ptr; unsigned int len;}));
  for (int i = 0; i < n_args; i++) {
    int sz = 0;
    for (char* x = _args[i]; *x; x++) sz++;
    args.ptr[i].len = sz;
    args.ptr[i].ptr = _args[i];
  }
  return (int)(long)rv_main(1, (void*)&args);
}
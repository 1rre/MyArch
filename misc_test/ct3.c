void* rv_fn_manage_println(int n_args, void* args);
void* rv_fn_manage_main(int n_args, void* args);
void* rv_fn_manage_PVq9rZm5G8qgZ16a(int n_args, void* args);
void* rv_fn_manage_println(int n_args, void* args) {
  switch (n_args) {
    case 1:
      struct rv_fnm_args_t0 {
        struct {unsigned char* ptr; unsigned int len;} s; // _0
      } *rv_fnm_args_0 = (struct rv_fnm_args_t0*)args;
      return rv_fn_manage_printf("%s\n",args->s);
      struct rv_fnm_args_t1 {
        signed int i; // _0
      } *rv_fnm_args_1 = (struct rv_fnm_args_t1*)args;
      return rv_fn_manage_printf("%d\n",i);
    break;
  }
}
void* rv_fn_manage_main(int n_args, void* args) {
  switch (n_args) {
    case 1:
      struct rv_fnm_args_t0 {
        struct {struct {unsigned char* ptr; unsigned int len;}* ptr; unsigned int len;} args; // _0
      } *rv_fnm_args_0 = (struct rv_fnm_args_t0*)args;
      {
  rv_fn_manage_for(args,rv_fn_manage_PVq9rZm5G8qgZ16a);
  return 5;
}
    break;
  }
}
void* rv_fn_manage_PVq9rZm5G8qgZ16a(int n_args, void* args) {
  switch (n_args) {
    case 1:
      struct rv_fnm_args_t0 {
        struct {unsigned char* ptr; unsigned int len;} a; // _0
      } *rv_fnm_args_0 = (struct rv_fnm_args_t0*)args;
      return rv_fn_manage_println(a);
    break;
  }
}


void* println(unsigned int n_args, void* args) {return rv_fn_manage_println(n_args, args);}
void* main(unsigned int n_args, void* args) {return rv_fn_manage_main(n_args, args);}
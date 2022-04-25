#include "micro_io.h"

#ifdef __myarch__

void print_c(char c) {
  #error "notimpl"
}

#else
#include <stdio.h>

void print_c(char c) {
  putchar(c);
}

#include <termios.h>
#include <sys/ioctl.h>
#include <unistd.h>

char input() {
  struct termios oldattr, newattr;
  char rtn;
  tcgetattr(STDIN_FILENO, &oldattr);
  newattr = oldattr;
  cfmakeraw(&newattr);
  newattr.c_lflag ^= ECHO;
  tcsetattr(STDIN_FILENO, TCSANOW, &newattr);
  rtn = getchar_unlocked();
  tcsetattr(STDIN_FILENO, TCSANOW, &oldattr);
  return rtn;
}

#endif


void print(char* c) {
  while (*c) print_c(*c++);
}


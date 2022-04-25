#include "micro_basic.h"
#include "micro_io.h"
#include "micro_malloc.h"

#define EOF_CHAR 4
#define BACKSP_CHAR 127
#define CR_CHAR '\r'

buffer_t read_program() {
  buffer_t buffer;
  char ch;
  int len;
  int blocks;
  buffer = (buffer_t)mmalloc(MALLOC_BLOCK_SIZE);
  blocks = 1;
  len = 0; /* for \0 */

  
input_init:
  ch = input();
  switch (ch) {
    case EOF_CHAR:
      return buffer;
    case BACKSP_CHAR:
      buffer[len] = 0;
      len--;
    break;
    case CR_CHAR:
      ch = '\n';
    default:
      if (MALLOC_BLOCK_SIZE - (len % MALLOC_BLOCK_SIZE) <= 2) {
        blocks++;
        buffer = (buffer_t)mrealloc(buffer, blocks * MALLOC_BLOCK_SIZE);
      }
      buffer[len] = ch;
      len++;
  }
  print("\e[0;0H\e[2J\e[3J");
  print(buffer);
  goto input_init;
}
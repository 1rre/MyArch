#include <termios.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <sys/ioctl.h>
#include <stdio.h>

#include "erl_nif.h"
#define str(x) #x
#define GOTO(x,y) "\e["str(x)";"str(y)"H"
#define CLEAR_SCREEN "\e[""3J\e[2J"
#define GOTO_START_LINE "\e[""0G"
#define CLEAR_LINE "\e[""2K"
#define CURSOR_UP "\e[""A"
#define CURSOR_DOWN "\e[""B"
#define CLEAR_TO_END "\e[""0J"

#define BUFFER_BLOCK_SIZE 80 * sizeof(char)

int col = 0;
int row = 0;

typedef struct {int len; int ptr; char* contents;} row_t;
typedef struct {int size; row_t* rows;} buffer_t;

buffer_t buffer = {0,0};


struct winsize old = {0,0,0};

void redraw_all(struct winsize w) {
  printf(
    CLEAR_SCREEN
    GOTO(0,0)
  );
  for (int i = 0; i < buffer.size; i++) {
    if (i) printf("\n");
    printf("%s", buffer.rows[i].contents);
    printf(CLEAR_TO_END);
  }
}

void draw_buffer() {
  struct winsize w;
  ioctl(0, TIOCGWINSZ, &w);
  if (w.ws_row != old.ws_row) redraw_all(w);
  else {
    int row = 0;
    for (int i = 0; i < buffer.size - 1; i++)
      row += (buffer.rows[i].ptr / w.ws_col) + 1;
    printf(
      CLEAR_LINE
      GOTO(%d, 0)
      "%s",
      row+1,
      buffer.rows[buffer.size - 1].contents
    );
  }
  old = w;
}

void grow_row(int n) {
  char* end = buffer.rows[n].contents + buffer.rows[n].len;
  buffer.rows[n].len += BUFFER_BLOCK_SIZE;
  buffer.rows[n].contents = realloc(buffer.rows[n].contents, buffer.rows[n].len);
  memset(end, 0, BUFFER_BLOCK_SIZE);
}

void shrink_row(int n) {
  buffer.rows[n].len -= BUFFER_BLOCK_SIZE;
  buffer.rows[n].contents = realloc(buffer.rows[n].contents, buffer.rows[n].len);
}

void add_row() {
  buffer.rows = realloc(buffer.rows, (buffer.size + 1) * sizeof(row_t));
  buffer.rows[buffer.size].len = BUFFER_BLOCK_SIZE;
  buffer.rows[buffer.size].ptr = 0;
  buffer.rows[buffer.size].contents = calloc(BUFFER_BLOCK_SIZE, sizeof(char));
  buffer.size++;
}

void rm_row() {
  buffer.size--;
  free(buffer.rows[buffer.size].contents);
  buffer.rows = realloc(buffer.rows, buffer.size * sizeof(row_t));
}

void do_put(int c) {
  if (buffer.size == 0) add_row();
  if (buffer.rows[buffer.size - 1].ptr + 3 >= buffer.rows[buffer.size - 1].len)
    grow_row(buffer.size - 1);
  (buffer.rows[buffer.size - 1].contents)[buffer.rows[buffer.size - 1].ptr] = c;
  buffer.rows[buffer.size - 1].ptr++;
  draw_buffer();
}

void do_newline() {
  printf(CURSOR_DOWN);
  add_row();
}

void do_bksp() {
  if (buffer.rows[buffer.size - 1].ptr == 0) {
    if (buffer.size > 1) rm_row();
    printf(CLEAR_LINE CURSOR_UP);
  } else {
    buffer.rows[buffer.size - 1].ptr--;
    buffer.rows[buffer.size - 1].contents[buffer.rows[buffer.size - 1].ptr] = 0;
  }
}

static ERL_NIF_TERM put_char(
    ErlNifEnv* env,
    int args_size,
    const ERL_NIF_TERM args[args_size]
  ) {
  int to_put;
  if (!enif_get_int(env, args[0], &to_put))
    return enif_make_badarg(env);
  if (to_put == 127) do_bksp();
  else if (to_put == '\n') do_newline();
  else do_put(to_put);
  draw_buffer();
  return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM get_char(
    ErlNifEnv* env,
    int args_size,
    const ERL_NIF_TERM args[args_size]
  ) {
  if (args_size != 0) return enif_make_badarg(env);
  
  struct termios oldattr, newattr;
  int ch;
  char s[2] = {0,0};

  tcgetattr( STDIN_FILENO, &oldattr );
  newattr = oldattr;
  cfmakeraw(&newattr);
  newattr.c_lflag = (0x8a3b | IXANY | IXOFF) & ~( ICANON | ECHO );
  tcsetattr( STDIN_FILENO, TCSANOW, &newattr );
  ch = getchar();
  if (ch == '\e') {
    getchar();
    getchar();
    if (getchar() == '1') getchar();
  } else {
    if (ch == '\r') ch = '\n';
    s[0] = ch;
  }
  tcsetattr( STDIN_FILENO, TCSANOW, &oldattr );
  return enif_make_string(env, s, ERL_NIF_LATIN1);
}



ERLC = erlc
CC = gcc
INCLUDEFLAGS = -I /usr/lib/erlang/usr/include/
OBJFLAGS = -fPIC -shared
CFLAGS = $(INCLUDEFLAGS) $(OBJFLAGS) -DERL

all: tty_client.beam tty_client.so tty_server.beam

%.so: %.c
	$(CC) -o $@ $(CFLAGS) $^

%.beam: %.erl
	$(ERLC) $^

clean:
	rm -rf *.beam *.so
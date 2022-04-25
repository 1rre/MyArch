-module(tty_client).
-export([
  init/0, start/0,
  get_char/0, put_char/1
]).
-on_load(init/0).

init() ->
  erlang:load_nif("./tty_client", 0).

put_char(_) -> erlang:nif_error("NIF library not loaded").
get_char() -> erlang:nif_error("NIF library not loaded").

listen() ->
  receive
    {put, C} -> put_char(C)
  end,
  listen().

start() ->
  spawn(fun () ->
    register(tty_client, self()),
    io:fwrite("\e[2J\e[3J\e[0;0H", []),
    listen()
  end),
  loop().

loop() ->
  timer:sleep(25),
  X = get_char(),
  if X =:= [3] -> halt(0); true -> ok end,
  {tty_server, 'tty_server@127.0.0.1'} ! {char, X},
  loop().

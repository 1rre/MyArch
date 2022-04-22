-module(myarch_shell).
-export([start/1]).

loop(MsgTo) ->
  Read = io:get_line(">> "),
  [MsgTo ! {char, C} || C <- Read],
  loop(MsgTo).

start(MsgTo) ->
  spawn(fun () -> loop(MsgTo) end).

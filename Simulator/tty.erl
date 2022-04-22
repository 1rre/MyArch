-module(tty).
-export([start/0]).
-include("defines.hrl").

-define(BUFFER_SIZE, 16#20000).

listen(Data, PRead, PWrite) ->
  receive
    {char, Value} ->
      <<St:PWrite/binary, _:8, End/bits>> = Data,
      listen(<<St:PWrite/binary, Value:8, End/bits>>, PRead, (PWrite + 1) rem (?BUFFER_SIZE div 8));
    {request, 1, _, From} when PRead =/= PWrite ->
      <<_:PRead/binary, Value:8, _/bits>> = Data,
      From ! {mem, <<Value:8>>},
      listen(Data, (PRead + 1) rem (?BUFFER_SIZE div 8), PWrite);
    {request, 1, _, From} ->
      From ! {mem, nodata},
      listen(Data, PRead, PWrite);
    {request, 0, _, From} ->
      N = if PRead =:= PWrite -> 0; true -> 1 end,
      From ! {mem, <<N:8>>},
      listen(Data, PRead, PWrite);
    Msg ->
      io:fwrite("Unexpected msg: ~p~n", [Msg])
  end.


start() ->
  Buffer = <<0:?BUFFER_SIZE>>,
  spawn(fun () -> listen(Buffer, 0, 0) end).

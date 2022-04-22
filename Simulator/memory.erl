-module(memory).
-export([start/0]).
-include("defines.hrl").

listen(Data) ->
  NewData = receive
  {store, Addr, Value} ->
    <<St:Addr/binary, _:(bit_size(Value)), End/binary>> = Data,
    <<St:Addr/binary, Value/binary, End/binary>>;
  {request, Addr, Width, From} ->
      Value = binary_part(Data, Addr, Width),
      From!{mem, Value},
      Data
  end,
  listen(NewData).


start() ->
  Data = <<0:16#20000>>,
  spawn(fun () -> listen(Data) end).

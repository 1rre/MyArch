-module(regfile).
-export([start/0, get/2, set/3]).
-include("defines.hrl").

listen(Data) ->
  NewData = receive
    {store, Addr, Value} ->
      StartSize = Addr * ?WORD_SIZE,
      <<Start:StartSize/bits, _:?WORD_SIZE, End/bits>> = Data,
      <<Start:StartSize/bits, Value/bits, End/bits>>;
    {request, Addr, From} ->
      StartSize = Addr * ?WORD_SIZE,
      <<_:StartSize/bits, Value:?WORD_SIZE/bits, _/bits>> = Data,
      From!{reg, Value},
      Data
  end,
  listen(NewData).

start() ->
  Data = <<0:?REG_SIZE>>,
  spawn(fun () -> listen(Data) end).

get(#{regfile := Reg}, Addr) ->
  Reg!{request, Addr, self()},
  receive
    {reg, Value} -> Value
  end.
set(#{regfile := Reg}, Addr, Data) ->
  Reg!{store, Addr, Data}.
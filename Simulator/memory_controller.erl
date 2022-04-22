-module(memory_controller).
-export([start/0, get/3, set/3, request/1, store/1]).
-include("defines.hrl").
-define(IN(A, X, Y), A >= X andalso A < Y).

request({request, Addr, #{ram := Ram}, Width, Resp})
    when ?IN(Addr, 16#40000000, 16#40020000 - Width) ->
  Ram!{request, Addr - 16#40000000, Width, Resp};
request({request, Addr, #{tty := Tty}, Width, Resp})
    when ?IN(Addr, 16#80000000, 16#80000010 - Width) ->
  Tty!{request, Addr - 16#80000000, Width, Resp};
request({request, Addr, _, _Width, Resp}) ->
  Resp!{mem, {out_of_range, Addr}}.

store({store, Addr, #{ram := Ram}, Data})
    when ?IN(Addr, 16#40000000, 16#40020000 - byte_size(Data)) ->
  Ram!{store, Addr - 16#40000000, Data};
store({store, Addr, #{tty := Tty}, Data})
    when ?IN(Addr, 16#80000000, 16#80000010 - byte_size(Data)) ->
  Tty!{store, Addr - 16#80000000, Data};
store({store, Addr, _Mem, _Data}) ->
  io:fwrite("Out of range: ~p~n", [Addr]),
  halt(1).
  

listen() ->
  receive
    Msg ->
      memory_controller:(element(1, Msg))(Msg)
  end,
  listen().

start() ->
  Controller = spawn(fun () -> listen() end),
  Ram = memory:start(),
  Tty = tty:start(),
  ssh_tool:start(Tty),
  #{controller => Controller, ram => Ram, tty => Tty}.
    
get(#{memory := Mem=#{controller := Ctrl}}, Addr, Width) ->
  Ctrl!{request, Addr, Mem, Width, self()},
  receive
    {mem, Value} -> Value
  end.

set(#{memory := Mem=#{controller := Ctrl}}, Addr, Data) ->
  Ctrl!{store, Addr, Mem, Data}.
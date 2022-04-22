-module(simulator).
-export([main/1, run/1, setup/0]).
-include("defines.hrl").

run(Prog) ->
  {ok, Bits} = file:read_file(Prog),
  io:fwrite("Running ~s~n", [Bits]).
 
setup() ->
  Reg = regfile:start(),
  Mem = memory_controller:start(),
  #{regfile => Reg, memory => Mem}.


main([File]) ->
  {ok, FileContent} = file:read_file(File),
  Parts = [element(2, io_lib:fread("~2u", X)) || X <- re:split(binary_to_list(FileContent), "\\s+", [{return, list}])],
  io:fwrite("~p~n", [Parts]).
-module(tty_server).
-export([start/0]).

start() ->
  io:fwrite("Registering ~p~n", [register(tty_server, self())]),
  recv_loop(<<>>).

recv_loop(Buffer) ->
  io:fwrite("Listening ~p~n", [Buffer]),
  NewBuffer = receive
    {char, [127]} when Buffer =/= <<>> ->
      {tty_client, 'tty_client@127.0.0.1'} ! {put, 127},
      <<NB:(byte_size(Buffer)-1)/binary, _/bits>> = Buffer,
      NB;
    {char, [C]} when C =/= 127 ->
      {tty_client, 'tty_client@127.0.0.1'} ! {put, C},
      <<Buffer/bits, C:8>>;
    {char, C} ->
      io:fwrite("Ignore ~p~n", [C]),
      Buffer;
    Msg -> 
      io:fwrite("Got ~p~n", [Msg]),
      Buffer
  end,
  recv_loop(NewBuffer).

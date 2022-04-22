-module(ssh_tool).
-export([start/1]).

start(Self) ->
	ssh:start(),
	ssh:daemon(8964, [{password, "passw"}, {shell, fun (_) -> myarch_shell:start(Self) end}]).

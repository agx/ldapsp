-module(ldapsp_log).
-export([
	 err/2,
	 warn/2,
	 info/2,
	 debug/2
]).

-spec info(string(), list()) -> ok.
info(Fmt, Args) ->
    io:fwrite("[I] Ldasp: " ++ Fmt, Args).

-spec warn(string(), list()) -> ok.
warn(Fmt, Args) ->
    io:fwrite("[W] Ldasp: " ++ Fmt, Args).

-spec err(string(), list()) -> ok.
err(Fmt, Args) ->
    io:fwrite("[E] Ldasp: " ++ Fmt, Args).

-spec debug(string(), list()) -> ok.
debug(Fmt, Args) ->
    io:fwrite("[D] Ldasp: " ++ Fmt, Args).


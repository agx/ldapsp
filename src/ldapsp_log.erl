-module(ldapsp_log).
-export([
	 err/2,
	 warn/2,
	 info/2,
	 debug/2
]).

-spec info(string(), list()) -> ok.
info(Fmt, Args) ->
    webmachine_log:log_info(io_lib:format(Fmt, Args)).

-spec warn(string(), list()) -> ok.
warn(Fmt, Args) ->
    webmachine_log:log_error(io_lib:format(Fmt, Args)).

-spec err(string(), list()) -> ok.
err(Fmt, Args) ->
    webmachine_log:log_error(io_lib:format(Fmt, Args)).

-spec debug(string(), list()) -> ok.
debug(Fmt, Args) ->
    webmachine_log:log_info(io_lib:format(Fmt, Args)).


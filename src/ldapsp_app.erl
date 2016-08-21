-module(ldapsp_app).

-behaviour(application).
-export([
    start/2,
    stop/1
]).

start(_Type, _StartArgs) ->
    LogHandlers = [{webmachine_access_log_handler, ["priv/log"]},
                   {webmachine_error_log_handler, ["priv/log"]}],
    application:set_env(webmachine, log_handlers, LogHandlers), 
    % Evil hack so we reprocess the app config
    webmachine:stop(),
    webmachine:start(),
    ldapsp_sup:start_link().

stop(_State) ->
    ok.

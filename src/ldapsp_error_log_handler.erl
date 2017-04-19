%%-----------------------------------------------------------------------
%% Copyright (c) 2017 Guido Günther
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% based on error_log_handler.erl
%% Copyright (c) 2011-2014 Basho Technologies, Inc.  All Rights Reserved.
%%-----------------------------------------------------------------------

%% @doc Log handler for ldapsp

-module(ldapsp_error_log_handler).

-behaviour(gen_event).

%% gen_event callbacks
-export([init/1,
         handle_call/2,
         handle_event/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(state, {filename, handle}).

-define(FILENAME, "ldapsp_error.log").

%% ===================================================================
%% gen_event callbacks
%% ===================================================================

%% @private
init([BaseDir]) ->
    {ok,_} = webmachine_log:defer_refresh(?MODULE),
    FileName = filename:join(BaseDir, ?FILENAME),
    Handle = log_open(FileName),
    {ok, #state{filename=FileName, handle=Handle}}.

%% @private
handle_call({_Label, MRef, get_modules}, State) ->
    {ok, {MRef, [?MODULE]}, State};
handle_call({refresh, _Time}, State) ->
    NewHandle = maybe_reopen(?MODULE,
			     State#state.filename,
			     State#state.handle),
    {ok, ok, State#state{handle=NewHandle}};
handle_call(_Request, State) ->
    {ok, ok, State}.

%% @private
handle_event({log_error, Msg}, State) ->
    NewHandle = maybe_reopen(?MODULE,
			     State#state.filename,
			     State#state.handle),
    NewState = State#state{handle=NewHandle},
    FormattedMsg = format_req(error, undefined, undefined, Msg),
    _ = webmachine_log:log_write(State#state.handle, FormattedMsg),
    {ok, NewState};
handle_event({log_error, Code, _Req, _Reason}, State) when Code < 500 ->
    {ok, State};
handle_event({log_error, Code, Req, Reason}, State) ->
    NewHandle = maybe_reopen(?MODULE,
			     State#state.filename,
			     State#state.handle),
    NewState = State#state{handle=NewHandle},
    Msg = format_req(error, Code, Req, Reason),
    _ = webmachine_log:log_write(State#state.handle, Msg),
    {ok, NewState};
handle_event({log_info, Msg}, State) ->
    NewHandle = maybe_reopen(?MODULE,
			     State#state.filename,
			     State#state.handle),
    NewState = State#state{handle=NewHandle},
    FormattedMsg = format_req(info, undefined, undefined, Msg),
    _ = webmachine_log:log_write(State#state.handle, FormattedMsg),
    {ok, NewState};
handle_event(_Event, State) ->
    {ok, State}.

%% @private
handle_info(_Info, State) ->
    {ok, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Internal functions
%% ===================================================================


format_req(Level, Code, Req, Msg) ->
    Time = webmachine_log:fmtnow(),
    format_req(Level, Time, Code, Req, Msg).

format_req(info, Time, undefined, _, Msg) ->
    [Time, " [info] ", Msg];
format_req(error, Time, undefined, _, Msg) ->
    [Time, " [error] ", Msg];
format_req(error, Time, 501, Req, _) ->
    {Path, _} = Req:path(),
    {Method, _} = Req:method(),
    Reason = "Webmachine does not support method ",
    [Time, " [error] ", Reason, Method, ": path=", Path, $\n];
format_req(error, Time, 503, Req, _) ->
    {Path, _} = Req:path(),
    Reason = "Webmachine cannot fulfill the request at this time",
    [Time, " [error] ", Reason, ": path=", Path, $\n];
format_req(error, Time, _Code, Req, Reason) ->
    {Path, _} = Req:path(),
    Str = io_lib:format("~p", [Reason]),
    [Time, " [error] ", "path=", Path, $\x20, Str, $\n].

%% @doc Open a new log file for writing
-spec log_open(string()) -> file:io_device().
log_open(LogName) ->
    error_logger:info_msg("opening log file: ~p~n", [LogName]),
    ok = filelib:ensure_dir(LogName),
    {ok, FD} = file:open(LogName, [read, write, raw]),
    {ok, Location} = file:position(FD, eof),
    webmachine_log:fix_log(FD, Location),
    ok = file:truncate(FD),
    FD.

%% @doc Rotate a log file if the hour it represents
%% has passed.
-spec maybe_reopen(atom(), string(), file:io_device()) -> file:io_device().
maybe_reopen(Mod, FileName, Handle) ->
    Reopen = not filelib:is_regular(FileName),
    maybe_reopen(Mod, FileName, Handle, Reopen).

-spec maybe_reopen(atom(), string(), file:io_device(), boolean()) ->
			  file:io_device().
maybe_reopen(_Mod, _FileName, Handle, false) ->
    Handle;
maybe_reopen(Mod, FileName, Handle, true) ->
    ok = webmachine_log:log_close(Mod, FileName, Handle),
    NewHandle = log_open(FileName),
    NewHandle.

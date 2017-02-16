%%-------------------------------------------------------------------
%% This file is part of ldapsp.
%%
%% Copyright (C) 2016 Guido Günther <agx@sigxcpu.org>
%%
%% ldapsp is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% ldapsp is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with ldapsp.  If not, see <http://www.gnu.org/licenses/>.
%%-------------------------------------------------------------------
-module(ldapsp_ldap).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3,
	 %% API
	 add/2,
	 delete/1
	]).

-define(SERVER, ?MODULE).

-record(state, {server, user, password}).

%%%===================================================================
%%% API
%%%===================================================================

-spec add(string(), list()) -> ok | {error,atom}.
add(Dn, Attributes) ->
    gen_server:call(?SERVER, {add, Dn, Attributes}).

-spec delete(string()) -> ok | {error,atom}.
delete(Dn) ->
    gen_server:call(?SERVER, {delete, Dn}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Args) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([{server, Server}, {user, User}, {password, Password}]) ->
    {ok, #state{server=Server,
		user=User,
		password=Password}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({add, Hostname, Class}, _From, State) ->
    Reply = do_add(Hostname, Class, State),
    {reply, Reply, State};
handle_call({delete, Hostname}, _From, State) ->
    Reply = do_delete(Hostname, State),
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

% -> ok , {error,Reaseon}
do_add(Dn, Attributes, #state{server=Server, user=User, password=Pw}) ->
    {ok, Handle} = eldap:open([Server]),
    ok = eldap:simple_bind(Handle, User, Pw),
    ldapsp_log:debug("Will create: ~p with ~p~n", [Dn, Attributes]),
    Resp = eldap:add(Handle, Dn, Attributes),
    check_close(eldap:close(Handle)),
    Resp.

% -> ok , {error,Reaseon}
do_delete(Dn, #state{server=Server, user=User, password=Pw}) ->
    {ok, Handle} = eldap:open([Server]),
    ok = eldap:simple_bind(Handle, User, Pw),
    ldapsp_log:debug("Will delete: ~p~n", [Dn]),
    Resp = eldap:delete(Handle, Dn),
    check_close(eldap:close(Handle)),
    Resp.

check_close(ok) -> ok;
% erlang 17.1 has another return value than 1.18.3
check_close({_Pid, close}) -> ok;
check_close(EverythingElse) -> ok = EverythingElse.
    

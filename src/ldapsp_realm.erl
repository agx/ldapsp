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
-module(ldapsp_realm).
-export([
	 allowed_methods/2,
	 content_types_provided/2,
	 init/1,
	 process_post/2,
	 routes/0
]).

-include_lib("webmachine/include/webmachine.hrl").

-spec init(list()) -> {ok, term()}.
init([]) ->
    {ok, undefined}.

allowed_methods(RD, Ctx) ->
    {['POST'], RD, Ctx}.

%% @doc Return the routes this module should respond to.
routes() ->
    [{["realm", realm], ?MODULE, []}].


content_types_provided(RD, Ctx) ->
    {[ {"text/html",        unused},
       {"application/json", unused} ],
     RD, Ctx}.

%% /realm/:realm/
-spec process_post(any(), any()) -> no_return().
process_post(RD, Ctx) ->
    Realm = wrq:path_info(realm, RD),

    Params = mochiweb_util:parse_qs(wrq:req_body(RD)),
    ldapsp_log:debug("Params: ~p~n", [Params]),
    Hostname = proplists:get_value("hostname", Params),
    Class = proplists:get_value("userclass", Params),
    Rebuild = proplists:get_value("rebuild", Params, false),
    ldapsp_log:info("Create host: Realm ~p, Hostname ~p, Class: ~p, Rebuild: ~p~n", [Realm, Hostname, Class, Rebuild]),
    Resp = ldapsp_proxy:add_host(Hostname, Class, Realm),
    Json = mochijson2:encode(Resp),
    {true, wrq:set_resp_body(Json, RD), Ctx}.

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
-module(ldapsp_realm_hostname).
-export([
	 allowed_methods/2,
	 content_types_provided/2,
	 delete_resource/2,
	 init/1,
	 routes/0
]).

-include_lib("webmachine/include/webmachine.hrl").

-spec init(list()) -> {ok, term()}.
init([]) ->
    {ok, undefined}.

allowed_methods(RD, Ctx) ->
    {['DELETE'], RD, Ctx}.

%% @doc Return the routes this module should respond to.
routes() ->
    [{["realm", realm, hostname], ?MODULE, []}].

content_types_provided(RD, Ctx) ->
    {[ {"text/html",        unused},
       {"application/json", unused} ],
     RD, Ctx}.

%% /realm/:realm/:hostname 
delete_resource(RD, Ctx) ->
    Realm = wrq:path_info(realm, RD),
    Hostname = wrq:path_info(hostname, RD),
    ldapsp_log:info("Delete host ~p in ~p ~n", [Hostname, Realm]),
    Ret = ldapsp_proxy:del_host(Hostname, Realm),
    {Ret, RD, Ctx}.


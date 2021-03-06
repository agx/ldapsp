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
-module(ldapsp_config).

-export([
	 dispatch/0,
	 web_config/0,
	 ldap_config/0,
	 policy_config/0
]).


-spec dispatch() -> [webmachine_dispatcher:route()].
dispatch() ->
    Resources = [ldapsp_features,
		 ldapsp_realm,
		 ldapsp_realm_hostname,
		 ldapsp_version
		],
    lists:flatten([Module:routes() || Module <- Resources]).

web_config() ->
    {ok, App} = application:get_application(?MODULE),
    {ok, Ip} = application:get_env(App, web_ip),
    {ok, Port} = application:get_env(App, web_port),
    [
     {ip, Ip},
     {port, Port},
     {log_dir, "priv/log"},
     {dispatch, dispatch()}
    ].

ldap_config() ->
    {ok, Config } = file:consult("priv/ldapsp.conf"),
    proplists:get_value(connection, Config).

policy_config() ->
    {ok, _Module} = compile:file("priv/policy.erl").
    

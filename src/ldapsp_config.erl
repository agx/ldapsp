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
		 ldapsp_realm_hostname
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
    Con = proplists:get_value(connection, Config),
    [{server, proplists:get_value(server, Con)},
     {user, proplists:get_value(user, Con)},
     {password, proplists:get_value(password, Con)}].

policy_config() ->
    {ok, _Module} = compile:file("priv/policy.erl").
    

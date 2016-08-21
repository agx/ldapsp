-module(ldapsp_sup).
-behaviour(supervisor).

%% External exports
-export([
  start_link/0
]).

-define(CHILD(I, Type, Args), {I, {I, start_link, [Args]}, permanent, 5000, Type, [I]}).

%% supervisor callbacks
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ldapsp_config:policy_config(),
    Web = {webmachine_mochiweb,
           {webmachine_mochiweb, start, [ldapsp_config:web_config()]},
           permanent, 5000, worker, [mochiweb_socket_server]},
    LdapArgs =  ldapsp_config:ldap_config(),
    Proxy = ?CHILD(ldapsp_ldap, worker, LdapArgs),
    Processes = [Web, Proxy],
    {ok, { {one_for_one, 10, 10}, Processes} }.

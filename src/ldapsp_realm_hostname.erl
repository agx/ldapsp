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


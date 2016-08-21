%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, Guido Günther
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(policy).

-export([add_host/3,
         del_host/2
	]).

% -> data to return
add_host(Hostname, Class, Realm) ->
    Dn = host2dn(Hostname, Realm),
    Attrs = class2attr(Hostname, Class, Realm),
    ok = ldapsp_ldap:add(Dn, Attrs),
    [{dn, list_to_binary(Dn)},
     {randompassword, <<"UNSET">>}].

% -> true, false
del_host(Hostname, Realm) ->
    Dn = host2dn(Hostname, Realm),
    del_result(ldapsp_ldap:delete(Dn)).

%% Private functions
host2dn(Host, Realm) ->
    Base = string:join([ "dc=" ++ C || C <- string:tokens(Realm, ".")], ", "),
    "cn=" ++ hd(string:tokens(Host, ".")) ++ ", " ++ Base.

class2attr(Host, _Class, Realm) ->
    [{"objectclass", ["top", "groupOfUniqueNames"]},
     {"cn", [Host]},
     {"uniqueMember", [host2dn(Host, Realm)]}].

del_result({error,noSuchObject}) -> true;
del_result(ok) -> true;
del_result(_) ->  false.


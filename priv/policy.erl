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

-define(POLICY_CONF, "priv/policy.conf").

% -> data to return
add_host(Hostname, Class, Realm) ->
    Dn = host2dn(Hostname, Realm),
    Members = members(Class),
    Attrs = class2attr(Hostname, Class, Realm, Members),
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

class2attr(Host, _Class, _Realm, Members) ->
    [{"objectclass", ["top", "groupOfUniqueNames"]},
     {"cn", [Host]},
     {"uniqueMember", Members}].


del_result({error,noSuchObject}) -> true;
del_result(ok) -> true;
del_result(_) ->  false.


members(Class, [{Dn,Classes}|Tail], Members) ->
    NewMembers = case lists:member(Class, Classes) of
		     true -> Members ++ [Dn];
		     _    -> Members
		 end,
    members(Class, Tail, NewMembers);
members(_Class, [], Members) -> Members.

members(Class) ->
    {ok, Config} = file:consult(?POLICY_CONF),

    Defaults = proplists:get_value(defaults, Config, []),
    Mappings = proplists:get_value(mappings, Config, []),

    Members = members(Class, Mappings, []),
    case Members of
	[] -> Defaults;
	_ -> Members
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

members_nonexistent_test() ->
    ?assertEqual(
       ["cn=default,dc=example,dc=com"],
       members("nonexistent")).

members_existent_test() ->
    ?assertEqual(
       ["cn=productA,dc=example,dc=com"],
       members("app1")).

members_multiple_test() ->
    ?assertEqual(
       ["cn=productA,dc=example,dc=com",
	"cn=productB,dc=example,dc=com"],
       members("app4")).

-endif.

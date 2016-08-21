%%%-------------------------------------------------------------------
%%% @author Guido <agx@sigxcpu.org>
%%% @copyright (C) 2016, Guido Günther
%%% @doc
%%%
%%% @end
%%% Created : 16 Aug 2016 by Guido <agx@sigxcpu.org>
%%%-------------------------------------------------------------------
-module(ldapsp_proxy).

-export([add_host/3,
         del_host/2
	]).

% -> ok , {error,Reaseon}
add_host(Hostname, Class, Realm) ->
    policy:add_host(Hostname, Class, Realm).

% -> ok , {error,Reaseon}
del_host(Hostname, Realm) ->
    policy:del_host(Hostname, Realm).


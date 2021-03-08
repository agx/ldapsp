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
    LdapArgs = ldapsp_config:ldap_config(),
    Proxy = ?CHILD(ldapsp_ldap, worker, LdapArgs),
    Processes = [Web, Proxy],
    {ok, { {one_for_one, 10, 10}, Processes} }.

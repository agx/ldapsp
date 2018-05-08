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
-module(ldapsp_version).
-export([
	 init/1,
	 routes/0,
	 to_html/2,
	 to_json/2,
         content_types_provided/2
]).

-include_lib("webmachine/include/webmachine.hrl").

-spec init(list()) -> {ok, term()}.
init([]) ->
    {ok, undefined}.

%% @doc Return the routes this module should respond to.
routes() ->
    [{["version"], ?MODULE, []}].

-spec to_json(wrq:reqdata(), term()) -> {iodata(), wrq:reqdata(), term()}.
to_json(ReqData, State) ->
    [Ver | _] = [ V || {App, _, V} <- application:which_applications(), App == ldapsp],
    Version = list_to_binary(Ver),
    Resp = mochijson2:encode([{"version", Version},
			      {"modules", [{"realm", Version}]}]),
    {Resp, ReqData, State}.

to_html(ReqData, State) ->
    to_json(ReqData, State).

content_types_provided(RD, Ctx) ->
   {[{"application/json", to_json},
     {"text/html", to_html}],
     RD, Ctx}.

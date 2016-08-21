-module(ldapsp_features).
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
    [{["features"], ?MODULE, []}].

-spec to_json(wrq:reqdata(), term()) -> {iodata(), wrq:reqdata(), term()}.
to_json(ReqData, State) ->
    Resp = mochijson2:encode([realm]),
    {Resp, ReqData, State}.

to_html(ReqData, State) ->
    to_json(ReqData, State).

content_types_provided(RD, Ctx) ->
   {[{"application/json", to_json},
     {"text/html", to_html}],
     RD, Ctx}.

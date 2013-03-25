-module(web_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    Data = get_metrics(),
    {ok, Req2} = cowboy_req:reply(200, [], Data, Req),
    {ok, Req2, State}.

get_metrics() ->
    jsx:encode([little_brother:dump_metrics()]).

terminate(_Reason, _Req, _State) ->
    ok.

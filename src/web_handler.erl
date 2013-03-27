-module(web_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    Data = case cowboy_req:path_info(Req) of
        {[], _} ->
            io:format("path is empty~n"),
            get_metrics();
        {[Path], _} ->
            Path1 = list_to_atom(binary_to_list(Path)),
            io:format("path is ~p~n", [Path1]),
            get_metrics(Path1)
    end,
    {ok, Req2} = cowboy_req:reply(200, [], Data, Req),
    {ok, Req2, State}.

get_metrics() ->
    jsx:encode(little_brother:dump_metrics()).
get_metrics(Path) ->
    jsx:encode(little_brother:dump_metrics(Path)).

terminate(_Reason, _Req, _State) ->
    ok.

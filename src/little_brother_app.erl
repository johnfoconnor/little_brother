-module(little_brother_app).

-behaviour(application).

-export([
         start/2,
         stop/1
        ]).

-define(APP, little_brother).


start(_Type, _Args) ->
    init_cowboy(),
    {ok, self()}.

stop(_State) ->
    ok.


init_cowboy() ->
    Dispatch = cowboy_router:compile([
            {'_', [{'_', web_handler, []}]}
    ]),
    {ok, _} = cowboy:start_http(http_listener, 100, [{port, 8080}], [
            {env, [{dispatch, Dispatch}]}
    ]),
    ok.



-module(little_brother_app).

-behaviour(application).

-export([
        start/0,
        stop/0,
         start/2,
         stop/1
        ]).

-define(APP, little_brother).

stop() ->
    application:stop(?APP).

start() ->
    application:start(folsom),
    application:start(?APP).

start(_Type, _Args) ->
    io:format("establishing link..~n"),
    case lb_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Other ->
            {error, Other}
    end.

stop(_State) ->
    ok.

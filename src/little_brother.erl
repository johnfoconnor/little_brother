-module(little_brother).

-export([start/0,
        start/1,
        update_metric/2,
        update_metric/1,
        dump_metrics/0,
        dump_metrics/1,
        print_metrics/0
    ]).


% Public API

start() ->
    io:format("starting without any metrics to track, not very useful. Try start/1~n"),
    launch().

% 
% {[{MetricName, MetricType, TypeSpec}],
%  [{Tag, MetricNames}]}
start(Spec) ->
    Ret = launch(),
    lb_adapter:init(Spec),
    Ret.

update_metric(Name, Tags) ->
    lb_adapter:safe_update_metric(Name, Tags).

update_metric(Name) ->
    update_metric(Name, []).

dump_metrics() ->
    lb_adapter:dump_metrics().

dump_metrics(Tag) ->
    lb_adapter:get_tagged_metrics(Tag).

print_metrics() ->
    lb_adapter:print_metrics().




% Private

launch() ->
    start_cowboy(),
    lb_adapter:start(),
    application:start(little_brother).

start_cowboy() ->
    start_dep(crypto),
    start_dep(ranch),
    start_dep(cowboy).

start_dep(Dep) ->
    io:format("starting dependency ~p\n", [Dep]),
    case application:start(Dep) of
        ok ->
            ok;
        Error ->
            io:format("    error: ~p\n",[Error])
    end.

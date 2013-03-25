-module(little_brother).

-export([start/1,
        notify_metric/1,
        increment_metric/1,
        decrement_metric/1,
        set_metric/1,
        dump_metrics/0,
        print_metrics/0
    ]).


% Public API

%% [{Metric_Name, Metric_type, Initial_value}]
start(Metrics) ->
    Ret = start(),
    lb_adapter:init_metrics(Metrics),
    Ret.


notify_metric(Metric) ->
    lb_adpater:notify_metric(Metric).

increment_metric(Metric) ->
    lb_adpater:increment_metric(Metric).

decrement_metric(Metric) ->
    lb_adpater:decrement_metric(Metric).

set_metric(Metric) ->
    lb_adapter:set_metric(Metric).

dump_metrics() ->
    lb_adapter:dump_metrics().

print_metrics() ->
    lb_adapter:print_metrics().



% Private

start() ->
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

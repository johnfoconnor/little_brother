-module(little_brother).

-export([start/0,
        start/1,
        get_metrics/0,
        get_metric_value/1,
        notify_metric/1,
        increment_metric/1,
        decrement_metric/1,
        set_metric/1,
        dump_metrics/0
    ]).

%% [{Metric_Name, Metric_type, Initial_value}]
start(Metrics) ->
    Ret = start(),
    init_metric(Metrics),
    Ret.

start() ->
    start_folsom(),
    start_cowboy(),
    application:start(little_brother).

start_folsom() ->
    start_dep(folsom).

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

get_metrics() ->
    folsom_metrics:get_metrics().

get_metric_value(Name) ->
    folsom_metrics:get_metric_value(Name).


dump_metrics() ->
    io:format("'Metric Name' => 'Metric Value'~n"), 
    dump_metrics(get_metrics()).

dump_metrics([]) ->
    ok;
dump_metrics([Name|Names]) ->
    io:format("    '~p' => '~p'~n", [Name, get_metric_value(Name)]),
    dump_metrics(Names).


init_metric([{Name, Type, Init}|Metrics]) ->
    Ret = new_metric(Type, Name),
    case Ret of
            ok ->
                notify_metric({Name, Init}),
                init_metric(Metrics);
            Error ->
                Error
        end;
init_metric([{Name, Type}|Metrics]) ->
        case new_metric(Type, Name) of
                ok ->
                    init_metric(Metrics);
                Error ->
                    Error
            end;
init_metric([]) ->
    ok;
 init_metric(_Metrics) ->
    {error, invalid_metric_spec}.

new_metric(Type, Name) ->
    create_new_metric(Type, Name).

create_new_metric(counter, Name) ->
    folsom_metrics:new_counter(Name);
create_new_metric(gauge, Name) ->
    folsom_metrics:new_gauge(Name);
create_new_metric(Type, _) ->
    {error, Type, unsupported_metric_type}.

notify_metric({Name, Args}) ->
    folsom_metrics:notify({Name, Args}).


increment_metric({Name, Amt}) -> 
    adjust_metric(Name, Amt, inc);
increment_metric(Name) -> 
    increment_metric({Name, 1}).

decrement_metric({Name, Amt}) -> 
    adjust_metric(Name, Amt, dec);
decrement_metric(Name) -> 
    decrement_metric({Name, 1}).

set_metric({Name, Amt}) ->
    notify_metric({Name, Amt});
set_metric(Name) -> 
    set_metric({Name, 1}).

adjust_metric(Name, Amt, Type) -> 
    notify_metric({Name, {Type, Amt}}).





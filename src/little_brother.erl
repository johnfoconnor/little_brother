-module(little_brother).

-export([get_metrics/0,
         get_metric_value/1,
         new_metric/2,
         notify_metric/1,
         dump_metrics/0
        ]).



get_metrics() ->
    folsom_metrics:get_metrics().

get_metric_value(Name) ->
    folsom_metrics:get_metric_value(Name).

dump_metrics() ->
    dump_metrics(get_metrics()).
dump_metrics([]) ->
    ok;
dump_metrics([Name, Names]) ->
    get_metric_value(Name),
    dump_metrics(Names).

new_metric(Type, Name) ->
    create_new_metric(Type, Name).

create_new_metric(counter, Name) -> 
    folsom_metrics:new_counter(Name);
create_new_metric(gauge, Name) -> 
    folsom_metrics:new_gauge(Name);
create_new_metric(Type, _) ->
    io:format("unsupported metric type ~p", Type).

notify_metric({Name, Args}) -> 
    folsom_metrics:notify({Name, Args}).


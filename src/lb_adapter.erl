-module(lb_adapter).

-export([start/0,
        init/1,
        new_histogram/1,
        new_metric/3,
        tag_metric/2,
        untag_metric/2,
        notify_metric/1,
        increment_metric/1,
        decrement_metric/1,
        adjust_metric/3,
        get_metrics/0,
        get_metric_value/1,
        get_tagged_metrics/1,
        get_tagged_metrics/2,
        dump_metrics/0,
        print_metrics/0
    ]).

-type metric() :: counter | gauge.
-define(DEFAULT_LIMIT, 5).
-define(DEFAULT_SIZE, 1028). % mimic codahale's metrics
-define(DEFAULT_SLIDING_WINDOW, 60). % sixty second sliding window
-define(DEFAULT_ALPHA, 0.015). % mimic codahale's metrics
-define(DEFAULT_INTERVAL, 5000).
-define(DEFAULT_SAMPLE_TYPE, uniform).


start() ->
    folsom:start().

%initialize metrics 

init({MetricSpec, TagSpec}) ->
    case init_metrics(MetricSpec) of
        ok ->
            init_tags(TagSpec);
        Error ->
            io:format("init error~n"),
            Error
    end.

init_tags([]) ->
    ok;
init_tags([{Tag, Names}|TagSpecs]) ->
    case tag_metrics(Names, Tag) of
        ok ->
            init_tags(TagSpecs);
        Error ->
            io:format("init tags error~n"),
            Error
    end.


tag_metrics([], _Tag) ->
    ok;
tag_metrics([Name|Names], Tag) ->
    case tag_metric(Name, Tag) of
        ok -> 
            tag_metrics(Names, Tag);
        Error ->
            io:format("error ~p tagging metric from spec ~p~n", [Error, {Name, Tag}]),
            Error
    end.


init_metrics([{Name, Type, TypeSpec}|Metrics]) ->
    case new_metric(Name, Type, TypeSpec) of
        ok ->
            init_metrics(Metrics);
        Error ->
            io:format("error ~p creating metric from spec ~p~n", [Error, {Name, Type, TypeSpec}]),
            Error
    end;
init_metrics([{Name, Type}|Metrics]) ->
    init_metrics([{Name, Type, []}, Metrics]);
init_metrics([]) ->
    ok;
init_metrics(_Metrics) ->
    {error, invalid_metric_spec}.

new_metric(Name, Type, TypeSpec) ->
    case Type of
        histogram ->
            new_histogram(list_to_tuple([Name|TypeSpec]));
        _ ->
            create_new_metric(Type, Name)
    end.

create_new_metric(counter, Name) ->
    folsom_metrics:new_counter(Name);
create_new_metric(gauge, Name) ->
    folsom_metrics:new_gauge(Name);
create_new_metric(meter, Name) ->
    folsom_metrics:new_meter(Name);
create_new_metric(spiral, Name) ->
    folsom_metrics:new_spiral(Name);
create_new_metric(meter_reader, Name) ->
    folsom_metrics:new_meter_reader(Name);
create_new_metric(duration, Name) ->
    folsom_metrics:new_duration(Name);
%% TODO
%% add support for histogram?
create_new_metric(Type, _) ->
    {error, Type, unsupported_metric_type}.

new_histogram({Name, SampleType, SampleSize, Alpha}) ->
    folsom_metrics:new_histogram(Name, SampleType, SampleSize, Alpha);
new_histogram({Name, SampleType, SampleSize}) ->
    new_histogram({Name, SampleType, SampleSize, ?DEFAULT_ALPHA});
new_histogram({Name, SampleType}) ->
    new_histogram({Name, SampleType, ?DEFAULT_SIZE, ?DEFAULT_ALPHA});
new_histogram({Name}) ->
    new_histogram({Name, ?DEFAULT_SAMPLE_TYPE, ?DEFAULT_SIZE, ?DEFAULT_ALPHA}).

% output metrics

print_metrics() ->
    io:format("~p~n", [lists:flatten(dump_metrics())]).

dump_metrics() ->
    dump_metrics(get_metrics(), "").

dump_metrics([], Output) ->
    Output;
dump_metrics([Name|Names], Output) ->
    Output1 = [{Name, get_metric_value(Name)}|Output],
    dump_metrics(Names, Output1).

get_metrics() ->
    folsom_metrics:get_metrics().

get_metric_value(Name) ->
    folsom_metrics:get_metric_value(Name).


% tagging metrics

get_tagged_metrics(Tag, Type) ->
    folsom_metrics:get_metrics_value(Tag, Type).

get_tagged_metrics(Tag) ->
    folsom_metrics:get_metrics_value(Tag).

tag_metric(Name, Tag) ->
    folsom_metrics:tag_metric(Name, Tag).

untag_metric(Name, Tag) ->
    folsom_metrics:untag_metric(Name, Tag).

% modify metrics

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

adjust_metric(Name, Amt, Type) -> 
    notify_metric({Name, {Type, Amt}}).

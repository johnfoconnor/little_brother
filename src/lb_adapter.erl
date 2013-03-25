-module(lb_adapter).

-export([start/0,
        init_metrics/1,
        new_metric/2,
        tag_metric/2,
        untag_metric/2,
        notify_metric/1,
        increment_metric/1,
        decrement_metric/1,
        set_metric/1,
        adjust_metric/3,
        get_metrics/0,
        get_metric_value/1,
        get_tagged_metrics/1,
        get_tagged_metrics/2,
        dump_metrics/0,
        print_metrics/0
    ]).

-type metric() :: counter | gauge.


start() ->
    folsom:start().

%initialize metrics 

init_metrics([{Name, Type, Init}|Metrics]) ->
    Ret = new_metric(Type, Name),
    case Ret of
            ok ->
                notify_metric({Name, Init}),
                init_metrics(Metrics);
            Error ->
                Error
        end;
init_metrics([{Name, Type}|Metrics]) ->
        case new_metric(Type, Name) of
                ok ->
                    init_metrics(Metrics);
                Error ->
                    Error
            end;
init_metrics([]) ->
    ok;
 init_metrics(_Metrics) ->
    {error, invalid_metric_spec}.

new_metric(Type, Name) ->
    create_new_metric(Type, Name).

create_new_metric(counter, Name) ->
    folsom_metrics:new_counter(Name);
create_new_metric(gauge, Name) ->
    folsom_metrics:new_gauge(Name);
create_new_metric(Type, _) ->
    {error, Type, unsupported_metric_type}.


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

set_metric({Name, Amt}) ->
    notify_metric({Name, Amt});
set_metric(Name) -> 
    set_metric({Name, 1}).

adjust_metric(Name, Amt, Type) -> 
    notify_metric({Name, {Type, Amt}}).

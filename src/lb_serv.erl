-module(lb_serv).

-behaviour(gen_server).

-export([start_link/0, new_metric/3, update_metric/3, print_metrics/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%public functions 

new_metric(Pid, Type, Name) ->
    gen_server:cast(Pid, {create_metric, {Type, Name}}).

update_metric(Pid, Name, Args) ->
    gen_server:cast(Pid, {notify_metric, {Name, Args}}).

print_metrics(Pid) ->
    gen_server:call(Pid, {dump_metrics}).

start_link() ->
    folsom:start(),
    InitArgs = [],
    gen_server:start_link(?MODULE, [], InitArgs).

% gen server functions

init([]) ->
    {ok, []}.

handle_cast({create_metric, {Type, Name}}, State) ->
    create_metric(Type, Name),
    {noreply, State};
handle_cast({notify_metric, Args}, State) ->
    notify_metric(Args),
    {noreply, State};
handle_cast(_Message, State) ->
    {noreply, State}.

handle_call({dump_metrics}, _From, State) ->
    {reply, dump_metrics(), State};
handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_info(Msg, State) ->
    io:format("Unexpected message: ~p~n",[Msg]),
    {noreply, State}.

terminate(normal, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    %% No change planned. The function is there for the behaviour,
    %% but will not be used. Only a version on the next
    {ok, State}.

% internal functions

get_metrics() ->
    folsom_metrics:get_metrics().

get_metric_value(Name) ->
    folsom_metrics:get_metric_value(Name).

dump_metrics() ->
    dump_metrics(get_metrics()).
dump_metrics([]) ->
    ok;
dump_metrics([Name|Names]) ->
    io:format("Metric: ~p = ~p~n", [Name, get_metric_value(Name)]),
    dump_metrics(Names).

create_metric(Type, Name) ->
    create_new_metric(Type, Name).

create_new_metric(counter, Name) -> 
    folsom_metrics:new_counter(Name);
create_new_metric(gauge, Name) -> 
    folsom_metrics:new_gauge(Name);
create_new_metric(Type, _) ->
    io:format("unsupported metric type ~p", Type).

notify_metric({Name, Args}) -> 
    folsom_metrics:notify({Name, Args}).


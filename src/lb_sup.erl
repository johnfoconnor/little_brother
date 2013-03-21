-module(lb_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    InitArgs = [],
    supervisor:start_link({local, ?SERVER}, ?MODULE, InitArgs).

init([]) ->
    ChildSpec = childSpec(),
    RestartStrategy = {simple_one_for_one, 0 , 1},
    {ok , {RestartStrategy, [ChildSpec]}}.

childSpec() -> 
    {little_brother_serv,  %% child id
        childStartFunction(), %% Start func
        transient, infinity,  %% restart type and shutdown timeout (ms)
        worker,  %% child type
        [lb_serv]}. %% modules

childStartFunction() ->
    {lb_serv, start_link, []}.

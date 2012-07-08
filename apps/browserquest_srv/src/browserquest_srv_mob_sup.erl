%%%-------------------------------------------------------------------
%%% @author Gustav Simonsson  <gustav.simonsson@gmail.com>
%%% @doc
%%%     Mob supervisor
%%% @end
%%% Created : 8 July 2012 by <gustav.simonsson@gmail.com>
%%%-------------------------------------------------------------------

-module(browserquest_srv_mob_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, add_child/4]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 3600,
    
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    %% All mobs are added after upstart when world is initialised.
    {ok, {SupFlags, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
add_child(Id, MobType, X, Y) ->
    Restart = permanent,
    Shutdown = 2000,
    ChildType = worker,
    
    Mob = 
        {erlang:make_ref(),
         {browserquest_srv_mob, start_link, [Id, MobType, X, Y]},
         Restart, Shutdown, ChildType, [browserquest_srv_mob]},
    {ok, Pid} = supervisor:start_child(?SERVER, Mob),
    Pid.

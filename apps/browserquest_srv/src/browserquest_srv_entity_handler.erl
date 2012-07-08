%%%-------------------------------------------------------------------
%%% @author Niclas Axelsson <burbas@Niclass-MacBook-Pro.local>
%%% @copyright (C) 2012, Niclas Axelsson
%%% @doc
%%%
%%% @end
%%% Created :  8 Jul 2012 by Niclas Axelsson <burbas@Niclass-MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(browserquest_srv_entity_handler).

-behaviour(gen_server).

-include("../include/browserquest.hrl").
%% API
-export([
	 start_link/0,
	 register/4,
	 register_static/3,
	 unregister/1,
	 event/3,
	 move_zone/2,
	 make_zone/2,
	 generate_id/1,
	 calculate_dmg/2,
	 get_target/1
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {
	  zones :: dict(),
	  targets :: dict(),
          mobs,
          staticEntities
	 }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

register(Zone, Type, Id, SpawnInfo) ->
    Pid = self(),
    gen_server:call(?MODULE, {register, Pid, Zone, Id}),
    event(Zone, Type, SpawnInfo).

register_static(Zone, Type, SpawnInfo) ->
    Pid = self(),
    gen_server:call(?MODULE, {register, {static, Pid}, Zone}),
    event(Zone, Type, SpawnInfo).

unregister(Zone) ->
    Pid = self(),
    gen_server:call(?MODULE, {unregister, Pid, Zone}).

event(Zone, Type, Message) ->
    Pid = self(),
    gen_server:call(?MODULE, {event, Pid, Zone, Type, Message}).

get_target(Target) ->
    gen_server:call(?MODULE, {get_target, Target}).

move_zone(OldZone, NewZone) ->
    Pid = self(),
    gen_server:call(?MODULE, {unregister, Pid, OldZone}),
    gen_server:call(?MODULE, {register, Pid, NewZone}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    Args = [fun add_mob/1, browserquest_srv_map:get_attribute("mobAreas")],
    erlang:spawn(lists, foreach, Args),
    {ok, #state{zones = dict:new(), targets = dict:new()}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({register, Pid, Zone, Id}, _From, State = #state{targets = Targets, zones = Zones}) ->
    UpdatedZones = dict:update(Zone, fun(Nodes) -> 
					     [Pid|Nodes] 
				     end, [Pid], Zones),

    UpdatedTargets = dict:store(Id, Pid, Targets),

    {reply, ok, State#state{zones = UpdatedZones, targets = UpdatedTargets}};

handle_call({unregister, Pid, Zone}, _From, State = #state{zones = Zones}) ->
    UpdatedZones = dict:update(Zone, fun(Nodes) ->
					     [ X || X <- Nodes, X /= Pid, X /= {static, Pid} ]
				     end, [], Zones),
    {reply, ok, State#state{zones = UpdatedZones}};

handle_call({get_target, Target}, _From, State = #state{targets = Targets}) ->
    Reply = dict:find(Target, Targets),
    {reply, Reply, State};

handle_call({event, Pid, Zone, Type, Message}, _From, State = #state{zones = Zones}) when is_pid(Pid) ->
    case dict:find(Zone, Zones) of
	{ok, Nodes} ->
	    [ gen_server:cast(Node, {event, Pid, Type, Message}) || Node <- Nodes, Node /= Pid, Node /= {static, Pid} ];
	_ ->
	    []
    end,
    {reply, ok, State};

handle_call({event, Target, _Zone, Type, Message}, _From, State = #state{targets = Targets}) ->
    case dict:find(Target, Targets) of
	{ok, Pid} ->
	    gen_server:cast(Pid, {event, Pid, Type, Message});
	_ ->
	    ok
    end,
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
ensure_bin(Int) when is_integer(Int) ->
    ensure_bin(erlang:integer_to_list(Int));
ensure_bin(List) when is_list(List) ->
    erlang:list_to_binary(List);
ensure_bin(Bin) when is_binary(Bin) ->
    Bin.

%%%===================================================================
%%% Exported functions
%%%===================================================================
make_zone(PosX, PosY) ->
    ZoneString = erlang:integer_to_list((( PosX div 28)+1)*((PosY div 12)+1)),
    ensure_bin("ZONE"++ZoneString).

generate_id(InitialValue) when is_list(InitialValue) ->
    random:seed(erlang:now()),
    InitialValue ++ erlang:integer_to_list(random:uniform(1000000)).

calculate_dmg(TargetArmor, SourceWeapon) ->
    Dealt = SourceWeapon * (5+random:uniform(5)),
    Absorbed = TargetArmor * (1+random:uniform(2)),
    case Dealt-Absorbed of
	Positive when Positive > 0 ->
	    Positive;
	_Neg ->
	    random:uniform(3)
    end.

add_mob(#mobarea{type = Type, x = X, y = Y}) ->
    browserquest_srv_mob_sup:add_child(Type, X, Y).

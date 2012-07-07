%%%-------------------------------------------------------------------
%%% @author Gustav Simonsson  <gustav.simonsson@gmail.com>
%%% @doc
%%% World game server for browserquest.
%%% @end
%%% Created : 7 July 2012 by <gustav.simonsson@gmail.com>
%%%-------------------------------------------------------------------
-module(browserquest_srv_worldserver).

-behaviour(gen_server).

-include("../include/browserquest.hrl").
%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-compile(export_all).

-define(SERVER, ?MODULE). 

-record(state, {id, maxplayers, websocketServer, updates_per_second = 50, map,
                entities = [], players = [], mobs = [], attackers = [],
                items = [], equipping = [], hurt = [], npcs = [],
                mobAreas = [], chestAreas = [], groups = [],
                outgoingQueues = [], itemCount = 0, playerCount = 0, 
                zoneGroupsReady = false}).

%%%===================================================================
%%% API
%%%===================================================================
start_link(Id, WebsocketServer) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Id, WebsocketServer], []).

%%%===================================================================
%%% Game API
%%%===================================================================
on_player_connect(Player) ->
    gen_server:call(?SERVER, {on_player_connect, Player}).

on_player_enter(Player) ->
    gen_server:call(?SERVER, {on_player_enter, Player}).

on_entity_attack(Attacker) ->
    gen_server:call(?SERVER, {on_entity_attack, Attacker}).

on_regen_tick() ->
    gen_server:call(?SERVER, {on_regen_tick}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([Id, WebsocketServer]) ->
    {ok, #state{id = Id, websocketServer = WebsocketServer}}.


handle_call({on_player_connect, Player}, _From, #state{map = Map}) ->
    {reply, ok, do_on_player_connect(Player, Map)};

handle_call({on_player_enter, Player}, _From, State) ->
    {reply, ok, do_on_player_enter(Player, State)};
handle_call({on_entity_attack, Attacker}, _From, State) ->
    {reply, do_on_entity_attack(Attacker, State)};
handle_call({on_regen_tick}, _From, State) ->
    {reply, do_on_regen_tick(State)};
handle_call(Request, From, State) ->
    browserquest_srv_util:unexpected_call(?MODULE, Request, From, State),
    Reply = ok,
    {reply, Reply, State}.

handle_cast(Msg, State) ->
    browserquest_srv_util:unexpected_cast(?MODULE, Msg, State),
    {noreply, State}.

handle_info(Info, State) ->
    browserquest_srv_util:unexpected_info(?MODULE, Info, State),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_on_player_connect(Player, Map) ->
    browserquest_srv_player:on_request_position(Player, Map).

do_on_player_enter(Player, State) ->
    lager:info("Player joined: ~p~n", [browserquest_srv_player:name(Player)]),
    PlayerCount = case browserquest_srv_player:has_entered_game(Player) of
                      false ->
                          State#state.playerCount + 1;
                      _ ->
                          State#state.playerCount
                  end,
    browserquest_srv_player:set_player_count(Player, PlayerCount),
    browserquest_srv_player:set_entities(Player, get_entities(Player, State)),

    %% Handle player movement, interaction with entities, world etc.
    State#state{playerCount = PlayerCount}.

%% Called when an entity is attacked by another entity
do_on_entity_attack(_Attacker, State) ->
    %% Todo
    State.

do_on_regen_tick(State) ->
    %% Handle player health regen
    State.

get_entities(_Player, _State) ->
    %% Todo entities list
    [].
    

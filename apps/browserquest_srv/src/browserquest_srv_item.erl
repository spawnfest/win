%%%-------------------------------------------------------------------
%%% @author Gustav Simonsson  <gustav.simonsson@gmail.com>
%%% @doc
%%% Browserquest server template
%%% @end
%%% Created : 7 July 2012 by <gustav.simonsson@gmail.com>
%%%-------------------------------------------------------------------
-module(browserquest_srv_item).

-behaviour(gen_server).

-include("../include/browserquest.hrl").
%% API
-export([create/2, start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-compile(export_all).

-define(SERVER, ?MODULE). 

-record(state, {id}).

%%%===================================================================
%%% API
%%%===================================================================
create(ItemType, Args) ->
    start_link(ItemType, Args).
start_link(ItemType, Args) ->
    gen_server:start_link(?MODULE, [ItemType, Args], []).

pickup(ItemPid, PlayerState) ->
    gen_server:call(ItemPid, {pickup, PlayerState}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([ItemType, Args]) ->
    browserquest_srv_entity_handler:register(Args),
    {ok, #state{id = ItemType}}.

handle_call({pickup, PlayerState}, _From, State = #state{id = Id}) ->
    {stop, normal, new_player_state(Id, PlayerState), State};        
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
new_player_state(ItemId, PS = #player_state{armor = Armor,
                                            weapon = Weapon,
                                            hitpoints = HP}) ->
    case item_id_to_type(ItemId) of
        armor -> PS#player_state{armor = Armor + 1};
        weapon -> PS#player_state{weapon = Weapon + 1};
        flask -> PS#player_state{hitpoints = HP + 25};
        burger -> PS#player_state{hitpoints = HP + 35};
        cake -> PS#player_state{hitpoints = HP + 55};
        %% Todo: invincibility?
        firepotion -> PS#player_state{hitpoints = HP + 75}
    end.
       
item_id_to_type(N) when N >= 20, N =< 26 ->
    armor;
item_id_to_type(N) when N >= 60, N =< 66 ->
    weapon;
item_id_to_type(35) -> flask;
item_id_to_type(36) -> burger;
item_id_to_type(38) -> firepotion;
item_id_to_type(39) -> cake.





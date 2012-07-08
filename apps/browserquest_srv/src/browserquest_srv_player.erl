%%%-------------------------------------------------------------------
%%% @author Gustav Simonsson  <gustav.simonsson@gmail.com>
%%% @doc
%%% Browserquest server template
%%% @end
%%% Created : 7 July 2012 by <gustav.simonsson@gmail.com>
%%%-------------------------------------------------------------------
-module(browserquest_srv_player).

-behaviour(gen_server).

-include("../include/browserquest.hrl").

%% API
-export([
	 start_link/3,
	 get_status/1,
	 move/3,
	 set_checkpoint/2,
	 update_zone/1,
	 get_zone/1,
	 get_surrondings/1,
	 chat/2,
	 stop/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-compile(export_all).

-define(CALC_HP(ArmorLevel), 80 + ((ArmorLevel - 1) * 30)).
-define(APP, browserquest_srv).

-record(state, {
	  id,
	  name,
	  armor,
	  weapon,
	  hitpoints,
	  pos_x,
	  pos_y,
	  checkpoint,
	  zone,
	  actionlist
	 }).


%%%===================================================================
%%% API
%%%===================================================================
start_link(Name, Armor, Weapon) ->
    gen_server:start_link(?MODULE, [Name, Armor, Weapon], []).

get_status(Pid) ->
    gen_server:call(Pid, {get_status}).

move(Pid, X, Y) ->
    gen_server:call(Pid, {move, X, Y}).

set_checkpoint(Pid, Value) ->
    gen_server:call(Pid, {set_checkpoint, Value}).

update_zone(Pid) ->
    gen_server:call(Pid, {update_zone}).

get_zone(Pid) ->
    gen_server:call(Pid, {get_zone}).

get_surrondings(Pid) ->
    gen_server:call(Pid, {get_surrondings}).

chat(Pid, Message) ->
    gen_server:call(Pid, {chat, Message}).

stop(Pid) ->
    gen_server:cast(Pid, {stop}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([Name, Armor, Weapon]) ->
    Id = generate_id(),
    Hitpoints = ?CALC_HP(Armor),
    {PosX, PosY} = {10,10},
    
    Zone = make_zone(PosX, PosY),

    {ok, #state{
       id = Id,
       name = Name,
       armor = Armor,
       weapon = Weapon,
       pos_x = PosX,
       pos_y = PosY,
       hitpoints = Hitpoints,
       checkpoint = 0,
       zone = Zone,
       actionlist = []
      }}.

handle_call({get_status}, _From, State = #state{id = Id, name = Name, zone = Zone, pos_x = X, pos_y = Y, hitpoints = HP, armor = Armor, weapon = Weapon}) ->
    browserquest_srv_player_handler:register(Zone, {action, [true, ?SPAWN, Id, ?WARRIOR, X, Y, Name, ?DOWN, Armor, Weapon]}),
    {reply, {ok, [Id, Name, X, Y, HP]}, State};

handle_call({move, X, Y}, _From, State = #state{pos_x = OldX, pos_y = OldY,
                                                id = Id, zone = Zone}) ->
    case browserquest_srv_map:is_out_of_bounds(X, Y) of
        true ->
            {reply, {ok, [Id, OldX, OldY]}, State};
        _ ->
            browserquest_srv_player_handler:event(
              Zone, {action, [?MOVE, Id, X, Y]}),
            {reply, {ok, [Id, X, Y]}, State#state{pos_x = X, pos_y = Y}}
    end;

handle_call({set_checkpoint, Value}, _From, State) ->
    {reply, ok, State#state{checkpoint = Value}};

handle_call({update_zone}, _From, State = #state{zone = OldZone, pos_x = X, pos_y = Y}) ->
    %% Delete old zone and insert the new one
    NewZone = make_zone(X, Y),
    browserquest_srv_player_handler:move_zone(OldZone, NewZone),
    {reply, ok, State#state{zone = NewZone}};

handle_call({get_zone}, _From, State = #state{zone = Zone}) ->
    {reply, {ok, Zone}, State};

handle_call({get_surrondings}, _From, State = #state{actionlist = ActionList}) ->
    {reply, ActionList, State#state{actionlist = []}};

handle_call({chat, Message}, _From, State = #state{id = Id, zone = Zone}) ->
    Action = [?CHAT, Id, Message],
    browserquest_srv_player_handler:event(Zone, {action, Action}),
    {reply, {ok, Action}, State};

handle_call(Request, From, State) ->
    browserquest_srv_util:unexpected_call(?MODULE, Request, From, State),
    Reply = ok,
    {reply, Reply, State}.

handle_cast({stop}, State) ->
    {stop, normal, State};

handle_cast({event, From, {action, [Initial,?SPAWN|Tl]}}, State = #state{id = Id, pos_x = X, pos_y = Y, name = Name, armor = Armor, weapon = Weapon, actionlist = ActionList}) ->
    case Initial of
	true ->
	    gen_server:cast(From, {event, self(), {action, [false, ?SPAWN, Id, ?WARRIOR, X, Y, Name, ?DOWN, Armor, Weapon]}});
	_ ->
	    ok
    end,
    lager:debug("Found a new entity"),
    {noreply, State#state{actionlist = [[?SPAWN|Tl]|ActionList]}};

handle_cast({event, _From, {action, AC}}, State = #state{actionlist = ActionList}) ->
    {noreply, State#state{actionlist = [AC|ActionList]}};

handle_cast(Msg, State) ->
    browserquest_srv_util:unexpected_cast(?MODULE, Msg, State),
    {noreply, State}.

handle_info(Info, State) ->
    browserquest_srv_util:unexpected_info(?MODULE, Info, State),
    {noreply, State}.

terminate(_Reason, #state{zone = Zone}) ->
    browserquest_srv_player_handler:unregister(Zone),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
generate_id() ->
    random:seed(erlang:now()),
    random:uniform(1000000).

make_zone(PosX, PosY) ->
    %%Zone = erlang:trunc(PosX/(PosX rem 28))*erlang:trunc(PosY/(PosY rem 11)),
    ZoneString = erlang:integer_to_list(PosX) ++ "x" ++ 
        erlang:integer_to_list(PosY) ++ "y",
    ensure_bin("ZONE"++ZoneString).

ensure_bin(Int) when is_integer(Int) ->
    ensure_bin(erlang:integer_to_list(Int));
ensure_bin(List) when is_list(List) ->
    erlang:list_to_binary(List);
ensure_bin(Bin) when is_binary(Bin) ->
    Bin.

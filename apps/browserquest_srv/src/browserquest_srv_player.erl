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
	 stop/1,
	 attack/2,
	 hit/2,
	 hurt/2
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-compile(export_all).

-define(CALC_HP(ArmorLevel), 80 + ((ArmorLevel - 1) * 30)).
-define(APP, browserquest_srv).



%%%===================================================================
%%% API
%%%===================================================================
start_link(Name, Armor, Weapon) ->
    gen_server:start_link(?MODULE, [Name, Armor, Weapon], []).

get_status(Pid) ->
    gen_server:call(Pid, {get_status}).

move(Pid, X, Y) ->
    gen_server:call(Pid, {move, X, Y}).

attack(Pid, Target) ->
    gen_server:call(Pid, {attack, Target}).

hit(Pid, Target) ->
    gen_server:call(Pid, {hit, Target}).

hurt(Pid, Attacker) ->
    gen_server:call(Pid, {hurt, Attacker}).

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
    Id = browserquest_srv_entity_handler:generate_id("5"),
    Hitpoints = ?CALC_HP(get_armor_lvl(Armor)),

    CPs = browserquest_srv_map:get_attribute("startingAreas"),
    random:seed(erlang:now()),
    #cp{x = PosX, y = PosY} = lists:nth(random:uniform(length(CPs)), CPs),
    
    Zone = browserquest_srv_entity_handler:make_zone(PosX, PosY),

    lager:debug("Player zone: ~p", [Zone]),
    {ok, #player_state{
       id = Id,
       name = Name,
       armor = Armor,
       weapon = Weapon,
       pos_x = PosX,
       pos_y = PosY,
       hitpoints = Hitpoints,
       checkpoint = 0,
       zone = Zone,
       actionlist = [],
       local_cache = []
      }}.

handle_call({get_status}, _From, State = #player_state{id = Id, name = Name, zone = Zone, pos_x = X, pos_y = Y, hitpoints = HP, armor = Armor, weapon = Weapon}) ->
    browserquest_srv_entity_handler:register(Zone, ?WARRIOR, Id, {action, [true, ?SPAWN, Id, ?WARRIOR, X, Y, Name, ?DOWN, Armor, Weapon]}),
    {reply, {ok, [Id, Name, X, Y, HP]}, State};

handle_call({move, X, Y}, _From, State = #player_state{pos_x = OldX, pos_y = OldY,
                                                id = Id, zone = Zone}) ->
    case browserquest_srv_map:is_out_of_bounds(X, Y) of
        true ->
	    lager:debug("Moved to ~p ~p", [X, Y]),
            {reply, {ok, [Id, OldX, OldY]}, State};
        _ ->
            browserquest_srv_entity_handler:event(
              Zone, ?WARRIOR, {action, [?MOVE, Id, X, Y]}),
            {reply, {ok, [Id, X, Y]}, State#player_state{pos_x = X, pos_y = Y}}
    end;

handle_call({set_checkpoint, Value}, _From, State) ->
    {reply, ok, State#player_state{checkpoint = Value}};

handle_call({update_zone}, _From, State = #player_state{zone = OldZone, id = Id, name = Name, armor = Armor, weapon = Weapon, pos_x = X, pos_y = Y}) ->
    %% Delete old zone and insert the new one
    NewZone = browserquest_srv_entity_handler:make_zone(X, Y),
    browserquest_srv_entity_handler:unregister(OldZone),
    browserquest_srv_entity_handler:register(NewZone, ?WARRIOR, Id, {action, [true, ?SPAWN, Id, ?WARRIOR, X, Y, Name, ?DOWN, Armor, Weapon]}),
    {reply, ok, State#player_state{zone = NewZone}};

handle_call({get_zone}, _From, State = #player_state{zone = Zone}) ->
    {reply, {ok, Zone}, State};

handle_call({get_surrondings}, _From, State = #player_state{actionlist = ActionList}) ->
    {reply, ActionList, State#player_state{actionlist = []}};

handle_call({chat, Message}, _From, State = #player_state{id = Id, zone = Zone}) ->
    Action = [?CHAT, Id, Message],
    browserquest_srv_entity_handler:event(Zone, ?WARRIOR, {action, Action}),
    {reply, {ok, Action}, State};

handle_call({attack, Target}, _From, State = #player_state{zone = Zone}) ->
    Action =
	case Target of
	    _IntTarget when is_integer(Target) ->
		[?ATTACK, erlang:integer_to_list(Target)];
	    _ ->
		[?ATTACK, Target]
	end,

    browserquest_srv_entity_handler:event(Zone, ?WARRIOR, {action, Action}),
    {reply, ok, State};

handle_call({hit, Target}, _From, State = #player_state{local_cache = {Target, {Id, _, Armor}}, weapon = Weapon}) ->
    Dmg = browserquest_srv_entity_handler:calculate_dmg(get_armor_lvl(Armor), get_weapon_lvl(Weapon)),
    browserquest_srv_mob:receive_damage(Target, Dmg),
    {reply, {ok, [?DAMAGE, Id, Dmg]}, State};

handle_call({hit, Target}, _From, State = #player_state{weapon = Weapon}) ->
    {ok, {Id, TargetWeapon, TargetArmor}} = browserquest_srv_mob:get_stats(Target),
    Dmg = browserquest_srv_entity_handler:calculate_dmg(get_armor_lvl(TargetArmor), get_weapon_lvl(Weapon)),
    browserquest_srv_mob:receive_damage(Target, Dmg),
    {reply, {ok, [?DAMAGE, Id, Dmg]}, State#player_state{local_cache = {Target, {Id, TargetWeapon, TargetArmor}}}};
   
handle_call({hurt, Attacker}, _From, State = #player_state{armor = Armor, hitpoints = HP, local_cache = {_Target, {Attacker, TargetWeapon, _}}}) ->
    Dmg = browserquest_srv_entity_handler:calculate_dmg(get_weapon_lvl(TargetWeapon), get_armor_lvl(Armor)),
    lager:debug("Received ~p damage. Have totally ~p", [Dmg, HP]),
    case HP-Dmg of
	Dead when Dead =< 0 ->
	    {reply, {ok, [?HEALTH, 0]}, State#player_state{hitpoints = 0}};
	TotalHP ->
	    {reply, {ok, [?HEALTH, TotalHP]}, State#player_state{hitpoints = TotalHP}}
    end;

handle_call({hurt, Attacker}, _From, State = #player_state{armor = Armor, hitpoints = HP}) ->
    {ok, {Id, TargetWeapon, TargetArmor}} = browserquest_srv_mob:get_stats(Attacker),
    Dmg = browserquest_srv_entity_handler:calculate_dmg(get_weapon_lvl(TargetWeapon), get_armor_lvl(Armor)),
    lager:debug("Received ~p damage. Have totally ~p", [Dmg, HP]),
    case HP-Dmg of
	Dead when Dead =< 0 ->
	    {reply, {ok, [?HEALTH, 0]}, State#player_state{hitpoints = 0, local_cache = {Attacker, {Id, TargetWeapon, TargetArmor}}}}; %%FIXME
	TotalHP ->
	    {reply, {ok, [?HEALTH, TotalHP]}, State#player_state{hitpoints = TotalHP, local_cache = {Attacker, {Id, TargetWeapon, TargetArmor}}}}
    end;

handle_call(Request, From, State) ->
    browserquest_srv_util:unexpected_call(?MODULE, Request, From, State),
    Reply = ok,
    {reply, Reply, State}.

handle_cast({stop}, State) ->
    {stop, normal, State};

handle_cast({event, From, _, {action, [Initial,?SPAWN|Tl]}}, State = #player_state{id = Id, pos_x = X, pos_y = Y, name = Name, armor = Armor, weapon = Weapon, actionlist = ActionList}) ->
    lager:debug("Action received: ~p", [[Initial,?SPAWN|Tl]]),
    case Initial of
	true ->
	    gen_server:cast(From, {event, self(), ?WARRIOR, {action, [false, ?SPAWN, Id, ?WARRIOR, X, Y, Name, ?DOWN, Armor, Weapon]}});
	_ ->
	    ok
    end,
    lager:debug("Found a new entity"),
    {noreply, State#player_state{actionlist = [[?SPAWN|Tl]|ActionList]}};

handle_cast({event, From, _, {action, [?ATTACK, Attacker]}}, State = #player_state{id = Id, actionlist = ActionList}) ->
    Action = [?ATTACK, Attacker, Id],
    {noreply, State#player_state{actionlist = [Action|ActionList]}};

handle_cast({event, _From, _Type, {action, AC}}, 
            State = #player_state{actionlist = ActionList}) ->
    lager:debug("Got event: ~p", [AC]),
    {noreply, State#player_state{actionlist = [AC|ActionList]}};

handle_cast(Msg, State) ->
    browserquest_srv_util:unexpected_cast(?MODULE, Msg, State),
    {noreply, State}.

handle_info(Info, State) ->
    browserquest_srv_util:unexpected_info(?MODULE, Info, State),
    {noreply, State}.

terminate(_Reason, #player_state{zone = Zone}) ->
    browserquest_srv_entity_handler:unregister(Zone),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_weapon_lvl(60) -> 1;
get_weapon_lvl(61) -> 2;
get_weapon_lvl(65) -> 3;
get_weapon_lvl(64) -> 4;
get_weapon_lvl(66) -> 5;
get_weapon_lvl(62) -> 6; 
get_weapon_lvl(63) -> 7;
get_weapon_lvl(P) -> P.

get_armor_lvl(21) -> 1;
get_armor_lvl(22) -> 2; 
get_armor_lvl(23) -> 3; 
get_armor_lvl(24) -> 4;
get_armor_lvl(25) -> 5; 
get_armor_lvl(26) -> 6;
get_armor_lvl(P) -> P.

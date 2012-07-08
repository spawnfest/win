%%%-------------------------------------------------------------------
%%% @author Sedrik <sir.sedrik@gmail.com>
%%% @doc
%%% Mob server for non player entities
%%% @end
%%% Created : 7 July 2012 by <sir.sedrik@gmail.com>
%%%-------------------------------------------------------------------
-module(browserquest_srv_mob).

-behaviour(gen_server).

-include("../include/browserquest.hrl").

%% API
-export([start_link/3, receive_damage/2, get_armor/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {id,
                type,
                hitpoints,
                pos_x, 
                pos_y,
                armor,
                weapon,
                hate,
		hate_counter,
                item,
                respawn_timout,
                return_timeout,
                orientation, %TODO initalize in init
                attackers = [],
                range,
                target,
		zone
            }).

%%%===================================================================
%%% API
%%%===================================================================
start_link(Type, X, Y) ->
    gen_server:start_link(?MODULE, [Type, X, Y], []).

%%%===================================================================
%%% Game API
%%%===================================================================
receive_damage(Pid, Amount) when is_pid(Pid) ->
    gen_server:cast(Pid, {receive_damage, Amount});
receive_damage(Target, Amount) ->
    {ok, Pid} = browserquest_srv_entity_handler:get_target(Target),
    receive_damage(Pid, Amount).

get_armor(Pid) when is_pid(Pid) ->
    lager:debug("Trying to get armor from Pid: ~p", [Pid]),
    gen_server:call(Pid, {get_armor});
get_armor(Target) ->
    {ok, Pid} = browserquest_srv_entity_handler:get_target(Target),
    get_armor(Pid).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([BinType, X, Y]) ->
    Id = browserquest_srv_entity_handler:generate_id("1"),
    Zone = browserquest_srv_entity_handler:make_zone(X, Y),
    Type = browserquest_srv_util:type_to_internal(BinType),
    Orientation = random:uniform(4),
    State = do_init(
	      Type, 
	      #state{id = Id, type = Type,
		     pos_x = X, pos_y = Y,
		     orientation = Orientation}
	     ),

    browserquest_srv_entity_handler:register(Zone, Type, Id, {action, [false,
                ?SPAWN, Id, Type, X, Y]}),
    {ok, State#state{zone = Zone, id = Id, type = Type}}.


handle_call({get_armor}, _From, State = #state{id = Id, armor = Armor}) ->
    {reply, {ok, {Id, Armor}}, State};

handle_call(Request, From, State) ->
    browserquest_srv_util:unexpected_call(?MODULE, Request, From, State),
    Reply = ok,
    {reply, ok, State}.

handle_cast({tick}, State = #state{hate = _Hate, hitpoints = HP}) ->
    case HP of
	Dead when Dead =< 0 ->
	    die;
	_ ->
	    ok
    end,
    {noreply, State};

handle_cast({event, From, ?WARRIOR, {action, [?MOVE, _Id, ?WARRIOR, X, Y, _Name, _Orient, _Armor, _Weapon]}}, State = #state{range = Range, pos_x = PX, pos_y = PY, hate = Hate}) when Hate =:= [] andalso ((PX-Range < X andalso X < (PX+Range)) orelse ((PY-Range) < Y andalso Y < (PY+Range))) ->
    %% Hates on for you
    {noreply, State#state{hate = [From]}};

%% A hero have spawned in our zone
handle_cast({event, From, ?WARRIOR, {action, [_, ?SPAWN, _Id, ?WARRIOR, _X, _Y, _Name, _Orient, _Armor, _Weapon]}}, State = #state{id = Id, type = Type, pos_x = X, pos_y = Y}) ->
    gen_server:cast(From, {event, self(), Id, {action, [false, ?SPAWN, Id, Type, X, Y]}}),
    {noreply, State};

handle_cast({event, From, ?WARRIOR, {action, [?ATTACK, Target]}}, State = #state{id = Target}) ->
    %% I'm gonna KILL you
    {noreply, State#state{hate = [From]}};

handle_cast({receive_damage, Amount}, 
            State = #state{id = Id, zone = Zone, type = Type, hitpoints = HP,
                           item = Item, pos_x = X, pos_y = Y}) ->
    lager:debug("Receiving damage: ~p", [Amount]),
    Total = HP - Amount,
    NewState = State#state{hitpoints = Total},
    case Total =< 0 of
        true ->
	    browserquest_srv_entity_handler:event(
              Zone, Type, {action, [?DESPAWN, Id]}),
	    browserquest_srv_entity_handler:unregister(Zone),
            drop_item(Item, X, Y),
	    {stop, normal, NewState};
        false ->
            {noreply, NewState}
    end;

handle_cast(Msg, State) ->
    browserquest_srv_util:unexpected_cast(?MODULE, Msg, State),
    {noreply, State}.

handle_info(Info, State) ->
    browserquest_srv_util:unexpected_info(?MODULE, Info, State),
    {noreply, State}.

terminate(_Reason, _State) ->
    timer:sleep(30000),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
drop_item(Item, X, Y) ->
    %% Items start with 9
    Id = browserquest_srv_entity_handler:generate_id("9"),
    Zone = browserquest_srv_entity_handler:make_zone(X,Y),
    Args = [Zone, Item, Id, [?SPAWN, Id, Item, X, Y]],
    %% Remove apply, spawn item gen_server, call register from it
    Fun = fun() -> browserquest_srv_item:create(Item, Args) end,
    spawn(Fun),
    ok.

%% Calculate the item dropped. The Item list needs to be sorted in ascending
%% order for it to work properly.
item([], _) -> undefined;
item([{Item, Chance} | _Items], Rand) when Rand =< Chance -> Item;
item([_ | Items], Rand) -> item(Items, Rand).

do_init(?RAT, State) ->
    Drops = [{?FIREPOTION, 5},
             {?BURGER, 15},
             {?FLASK, 55}],

    State#state{hitpoints = 25,
                item = item(Drops, random:uniform(100)),
                armor = 1,
		range = 1,
                weapon = 1};
do_init(?SKELETON, State) ->
    Drops = [{?FIREPOTION, 5},
             {?AXE, 25},
             {?MAILARMOR, 35},
             {?FLASK, 75}],

    State#state{hitpoints = 110,
                item = item(Drops, random:uniform(100)),
                armor = 2,
		range = 3,
                weapon = 2};
do_init(?GOBLIN, State) ->
    Drops = [{?FIREPOTION, 5},
             {?AXE, 15},
             {?MAILARMOR, 35},
             {?FLASK, 75}],

    State#state{hitpoints = 90,
                item = item(Drops, random:uniform(100)),
                armor = 2,
		range = 3,
                weapon = 1};
do_init(?OGRE, State) ->
    Drops = [{?FIREPOTION, 5},
             {?BURGER, 15},
             {?PLATEARMOR, 35},
             {?MORNINGSTAR, 55},
             {?FLASK, 100}],

    State#state{hitpoints = 200,
                item = item(Drops, random:uniform(100)),
                armor = 3,
		range = 3,
                weapon = 2};
do_init(?SPECTRE, State) ->
    Drops = [{?FIREPOTION, 5},
             {?REDSWORD, 35},
             {?FLASK, 65},
             {?REDARMOR, 100}],

    State#state{hitpoints = 250,
                item = item(Drops, random:uniform(100)),
                armor = 2,
		range = 2,
                weapon = 4};
do_init(?DEATHKNIGHT, State) ->
    Drops = [{?BURGER, 95},
             {?FIREPOTION, 100}],

    State#state{hitpoints = 250,
                item = item(Drops, random:uniform(100)),
                armor = 3,
		range = 5,
                weapon = 3};
do_init(?CRAB, State) ->
    Drops = [{?FIREPOTION, 5},
             {?LEATHERARMOR, 15},
             {?AXE, 35},
             {?FLASK, 85}],

    State#state{hitpoints = 60,
                item = item(Drops, random:uniform(100)),
                armor = 2,
		range = 5,
                weapon = 1};
do_init(?SNAKE, State) ->
    Drops = [{?FIREPOTION, 5},
             {?MORNINGSTAR, 15},
             {?MAILARMOR, 25},
             {?FLASK, 75}],

    State#state{hitpoints = 60,
                item = item(Drops, random:uniform(100)),
                armor = 2,
		range = 3,
                weapon = 1};
do_init(?SKELETON2, State) ->
    Drops = [{?FIREPOTION, 5},
             {?BLUESWORD, 20},
             {?PLATEARMOR, 35},
             {?FLASK, 95}],

    State#state{hitpoints = 200,
                item = item(Drops, random:uniform(100)),
                armor = 3,
		range = 4,
                weapon = 3};
do_init(?EYE, State) ->
    Drops = [{?FIREPOTION, 5},
             {?REDSWORD, 15},
             {?REDARMOR, 35},
             {?FLASK, 85}],

    State#state{hitpoints = 200,
                item = item(Drops, random:uniform(100)),
                armor = 3,
		range = 1,
                weapon = 3};
do_init(?BAT, State) ->
    Drops = [{?FIREPOTION, 5},
             {?AXE, 15},
             {?FLASK, 65}],

    State#state{hitpoints = 80,
                item = item(Drops, random:uniform(100)),
                armor = 2,
		range = 2,
                weapon = 1};
do_init(?WIZARD, State) ->
    Drops = [{?FIREPOTION, 5},
             {?PLATEARMOR, 25},
             {?FLASK, 75}],

    State#state{hitpoints = 100,
                item = item(Drops, random:uniform(100)),
                armor = 2,
		range = 5,
                weapon = 6};
do_init(?BOSS, State) ->
    State#state{hitpoints = 100,
                item = ?GOLDENSWORD,
                armor = 2,
		range = 9,
                weapon = 6};
do_init(_Type, State) ->
    lager:error("Unknown mob type initialization"),
    State.


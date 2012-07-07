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
-export([start_link/2, receive_damage/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {id,
                hitpoints,
                pos_x, pos_y,
                armor,
                weapon,
                hate,
                item,
                respawn_timout,
                return_timeout,
                is_dead = false,
                orientation, %TODO initalize in init
                attackers = [],
                target
            }).

%%%===================================================================
%%% API
%%%===================================================================
start_link(Id, WebsocketServer) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Id, WebsocketServer], []).

%%%===================================================================
%%% Game API
%%%===================================================================
receive_damage(Amounth) ->
    gen_server:cast(?SERVER, {receive_damage, Amounth}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([Id, Type, X, Y]) ->
    %TODO check what to get from mob's Super in js
%    State = #state{id = Id, type = Type, pos_x = X, pos_y = Y},
%    {ok, do_init(Type, State)}.
    ok.


%handle_call({on_player_connect, Player}, _From, #state{map = Map}) ->
%    {reply, ok, do_on_player_connect(Player, Map)};

handle_call(Request, From, State) ->
    browserquest_srv_util:unexpected_call(?MODULE, Request, From, State),
    Reply = ok,
    {reply, Reply, State}.

handle_cast({receive_damage, Amounth}, State) ->
%    {reply, ok, do_receive_damage(Amounth, State)}.
%handle_cast(Msg, State) ->
 %   browserquest_srv_util:unexpected_cast(?MODULE, Msg, State),
    {noreply, State}.

handle_info(Info, State) ->
    browserquest_srv_util:unexpected_info(?MODULE, Info, State),
    {noreply, State}.

terminate(_Reason, _State) ->
    %TODO DEATH!
    %destroy: function() {
    %    this.isDead = true;
    %    this.hatelist = [];
    %    this.clearTarget();
    %    this.updateHitPoints();
    %    this.resetPosition();
    %    
    %    this.handleRespawn();
    %},
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Calculate the item dropped. The Item list needs to be sorted in ascending
%% order for it to work properly.
item([], _) -> undefined;
%item([{Item, Chance} | Items], Rand) if Rand <= Chance ->
%        Item;
item([_ | Items], Rand) ->
    item(Items, Rand).

do_init(?RAT, State) ->
    Drops = [{?FIREPOTION, 5},
             {?BURGER, 15},
             {?FLASK, 55}],

    State#state{hitpoints = 25,
                item = item(Drops, random:uniform(100)),
                armor = 1,
                weapon = 1};
do_init(?SKELETON, State) ->
    Drops = [{?FIREPOTION, 5},
             {?AXE, 25},
             {?MAILARMOR, 35},
             {?FLASK, 75}],

    State#state{hitpoints = 110,
                item = item(Drops, random:uniform(100)),
                armor = 2,
                weapon = 2};
do_init(_Type, State) ->
    lager:error("Unknown mob type initialization"),
    State.

%TODO Translate initializations
    %goblin: {
    %    drops: {
    %        flask: 50,
    %        leatherarmor: 20,
    %        axe: 10,
    %        firepotion: 5
    %    },
    %    hp: 90,
    %    armor: 2,
    %    weapon: 1
    %},
    %
    %ogre: {
    %    drops: {
    %        burger: 10,
    %        flask: 50,
    %        platearmor: 20,
    %        morningstar: 20,
    %        firepotion: 5
    %    },
    %    hp: 200,
    %    armor: 3,
    %    weapon: 2
    %},
    %
    %spectre: {
    %    drops: {
    %        flask: 30,
    %        redarmor: 40,
    %        redsword: 30,
    %        firepotion: 5
    %    },
    %    hp: 250,
    %    armor: 2,
    %    weapon: 4
    %},
    %
    %deathknight: {
    %    drops: {
    %        burger: 95,
    %        firepotion: 5
    %    },
    %    hp: 250,
    %    armor: 3,
    %    weapon: 3
    %},
    %
    %crab: {
    %    drops: {
    %        flask: 50,
    %        axe: 20,
    %        leatherarmor: 10,
    %        firepotion: 5
    %    },
    %    hp: 60,
    %    armor: 2,
    %    weapon: 1
    %},
    %
    %snake: {
    %    drops: {
    %        flask: 50,
    %        mailarmor: 10,
    %        morningstar: 10,
    %        firepotion: 5
    %    },
    %    hp: 150,
    %    armor: 3,
    %    weapon: 2
    %},
    %
    %skeleton2: {
    %    drops: {
    %        flask: 60,
    %        platearmor: 15,
    %        bluesword: 15,
    %        firepotion: 5
    %    },
    %    hp: 200,
    %    armor: 3,
    %    weapon: 3
    %},
    %
    %eye: {
    %    drops: {
    %        flask: 50,
    %        redarmor: 20,
    %        redsword: 10,
    %        firepotion: 5
    %    },
    %    hp: 200,
    %    armor: 3,
    %    weapon: 3
    %},
    %
    %bat: {
    %    drops: {
    %        flask: 50,
    %        axe: 10,
    %        firepotion: 5
    %    },
    %    hp: 80,
    %    armor: 2,
    %    weapon: 1
    %},
    %
    %wizard: {
    %    drops: {
    %        flask: 50,
    %        platearmor: 20,
    %        firepotion: 5
    %    },
    %    hp: 100,
    %    armor: 2,
    %    weapon: 6
    %},
    %
    %boss: {
    %    drops: {
    %        goldensword: 100
    %    },
    %    hp: 700,
    %    armor: 6,
    %    weapon: 7
    %}

do_receive_damage(Amounth, State) ->
%    State#state.hitpoints -= Amounth,
%    case State#state.hitpoints <= 0 of
%        true -> %DEATH!
%            State#state.is_dead = true;
%        false ->
            State.
%    end.

    %hates: function(playerId) {
    %    return _.any(this.hatelist, function(obj) { 
    %        return obj.id === playerId; 
    %    });
    %},
    %
    %increaseHateFor: function(playerId, points) {
    %    if(this.hates(playerId)) {
    %        _.detect(this.hatelist, function(obj) {
    %            return obj.id === playerId;
    %        }).hate += points;
    %    }
    %    else {
    %        this.hatelist.push({ id: playerId, hate: points });
    %    }

    %    /*
    %    log.debug("Hatelist : "+this.id);
    %    _.each(this.hatelist, function(obj) {
    %        log.debug(obj.id + " -> " + obj.hate);
    %    });*/
    %    
    %    if(this.returnTimeout) {
    %        // Prevent the mob from returning to its spawning position
    %        // since it has aggroed a new player
    %        clearTimeout(this.returnTimeout);
    %        this.returnTimeout = null;
    %    }
    %},
    
    %getHatedPlayerId: function(hateRank) {
    %    var i, playerId,
    %        sorted = _.sortBy(this.hatelist, function(obj) { return obj.hate; }),
    %        size = _.size(this.hatelist);
    %    
    %    if(hateRank && hateRank <= size) {
    %        i = size - hateRank;
    %    }
    %    else {
    %        i = size - 1;
    %    }
    %    if(sorted && sorted[i]) {
    %        playerId = sorted[i].id;
    %    }
    %    
    %    return playerId;
    %},
    
    %forgetPlayer: function(playerId, duration) {
    %    this.hatelist = _.reject(this.hatelist, function(obj) { return obj.id === playerId; });
    %    
    %    if(this.hatelist.length === 0) {
    %        this.returnToSpawningPosition(duration);
    %    }
    %},
    
    %forgetEveryone: function() {
    %    this.hatelist = [];
    %    this.returnToSpawningPosition(1);
    %},
    
    %drop: function(item) {
    %    if(item) {
    %        return new Messages.Drop(this, item);
    %    }
    %},
    
    %handleRespawn: function() {
    %    var delay = 30000,
    %        self = this;
    %    
    %    if(this.area && this.area instanceof MobArea) {
    %        // Respawn inside the area if part of a MobArea
    %        this.area.respawnMob(this, delay);
    %    }
    %    else {
    %        if(this.area && this.area instanceof ChestArea) {
    %            this.area.removeFromArea(this);
    %        }
    %        
    %        setTimeout(function() {
    %            if(self.respawn_callback) {
    %                self.respawn_callback();
    %            }
    %        }, delay);
    %    }
    %},
    
    %resetPosition: function() {
    %    this.setPosition(this.spawningX, this.spawningY);
    %},
    
    %returnToSpawningPosition: function(waitDuration) {
    %    var self = this,
    %        delay = waitDuration || 4000;
    %    
    %    this.clearTarget();
    %    
    %    this.returnTimeout = setTimeout(function() {
    %        self.resetPosition();
    %        self.move(self.x, self.y);
    %    }, delay);
    %},
    
    %move: function(x, y) {
    %    this.setPosition(x, y);
    %    if(this.move_callback) {
    %        this.move_callback(this);
    %    }
    %},
    
    %updateHitPoints: function() {
    %    this.resetHitPoints(Properties.getHitPoints(this.kind));
    %},
    
    %distanceToSpawningPoint: function(x, y) {
    %    return Utils.distanceTo(x, y, this.spawningX, this.spawningY);
    %}

    %getState: function() {
    %    var basestate = this._getBaseState(),
    %        state = [];
    %    
    %    state.push(this.orientation);
    %    if(this.target) {
    %        state.push(this.target);
    %    }
    %    
    %    return basestate.concat(state);
    %},
    
    %regenHealthBy: function(value) {
    %    var hp = this.hitPoints,
    %        max = this.maxHitPoints;
    %        
    %    if(hp < max) {
    %        if(hp + value <= max) {
    %            this.hitPoints += value;
    %        }
    %        else {
    %            this.hitPoints = max;
    %        }
    %    }
    %},
    
    %hasFullHealth: function() {
    %    return this.hitPoints === this.maxHitPoints;
    %},
    
    %setTarget: function(entity) {
    %    this.target = entity.id;
    %},
    
    %clearTarget: function() {
    %    this.target = null;
    %},
    
    %hasTarget: function() {
    %    return this.target !== null;
    %},

    %attack: function() {
    %    return new Messages.Attack(this.id, this.target);
    %},

    %health: function() {
    %    return new Messages.Health(this.hitPoints, false);
    %},

    %regen: function() {
    %    return new Messages.Health(this.hitPoints, true);
    %},

    %addAttacker: function(entity) {
    %    if(entity) {
    %        this.attackers[entity.id] = entity;
    %    }
    %},

    %removeAttacker: function(entity) {
    %    if(entity && entity.id in this.attackers) {
    %        delete this.attackers[entity.id];
    %        log.debug(this.id +" REMOVED ATTACKER "+ entity.id);
    %    }
    %},

    %forEachAttacker: function(callback) {
    %    for(var id in this.attackers) {
    %        callback(this.attackers[id]);
    %    }
    %}

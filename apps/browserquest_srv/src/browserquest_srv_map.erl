%%%-------------------------------------------------------------------
%%% @author Gustav Simonsson  <gustav.simonsson@gmail.com>
%%% @doc
%%% Server-side map logic
%%% @end
%%% Created : 7 July 2012 by <gustav.simonsson@gmail.com>
%%%-------------------------------------------------------------------
-module(browserquest_srv_map).

-behaviour(gen_server).

-include("../include/browserquest.hrl").
%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-compile(export_all).

-define(SERVER, ?MODULE). 

%%%===================================================================
%%% API
%%%===================================================================
start_link(FilePath) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [FilePath], []).

%%%===================================================================
%%% Game API
%%%===================================================================
get_attribute(Attribute) ->
    gen_server:call(?SERVER, {get_attribute, Attribute}).

is_colliding(X, Y) ->
    gen_server:call(?SERVER, {is_colliding, X, Y}).

is_out_of_bounds(X, Y) ->
    gen_server:call(?SERVER, {is_out_of_bounds, X, Y}).

tileid_to_pos(TileId) ->
    gen_server:call(?SERVER, {tileid_to_pos, TileId}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([MapName]) ->
    File = code:priv_dir(browserquest_srv) ++ "/maps/" ++ MapName,
    {ok, FileBin} = file:read_file(File),
    Json = mochijson3:decode(FileBin),
    Height = get_json_value("height", Json),  
    Width = get_json_value("width", Json),
    ZoneWidth = 28,
    ZoneHeight = 12,

    Grid = get_grid(Height, Width, get_json_value("collisions", Json)),
    TileToPos = grid_to_tileid_lookup(Grid),
    
    Checkpoints = lists:map(fun get_checkpoint/1,
                            get_json_value("checkpoints", Json)),

    %% Change this to a subset of checkpoints? Mysterious .s field
    %% in cp in map.js.
    StartingAreas = Checkpoints,

    MobAreas = lists:map(fun get_mobarea/1,
                         get_json_value("roamingAreas", Json)),

    {_,StaticEntities} = get_json_value("staticEntities", Json),
                            
    PropList = [{"width", Width}, {"height", Height},
                {"zoneWidth", ZoneWidth}, {"zoneHeight", ZoneHeight},
                {"groupWidth", trunc(Width / ZoneWidth)},
                {"groupHeight", trunc(Height / ZoneHeight)},
                {"grid", Grid},
                {"tiletopos", TileToPos},
                {"startingAreas", StartingAreas},
                {"checkpoints", Checkpoints},
                {"mobAreas", MobAreas},
                {"staticEntities", StaticEntities}
               ],
    
    Map = #map{json = Json, attributes = PropList},
    {ok, Map}.

handle_call({get_attribute, Attribute}, _From, Map) ->
    {reply, do_get_attribute(Attribute, Map), Map};
handle_call({is_colliding, X, Y}, _From, #map{attributes = PL} = Map) ->
    Grid = proplists:get_value("grid", PL),
    {reply, do_is_colliding(X, Y, Grid), Map};
handle_call({is_out_of_bounds, X, Y}, _From, #map{attributes = PL} = Map) ->
    {reply, do_is_out_of_bounds(X, Y, PL), Map};
handle_call({tileid_to_pos, TileId}, _From, #map{attributes = PL} = Map) ->
    TileToPos = proplists:get_value("tiletopos", PL),
    {reply, tileid_to_pos(TileId, TileToPos), Map};
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
do_get_attribute(Attribute, #map{json = Json, attributes = PL}) ->
    case proplists:is_defined(Attribute, PL) of
        true ->
            proplists:get_value(Attribute, PL);
        _ ->
            get_json_value(Attribute, Json)
    end.

get_json_value(Key, Json) ->
    mochijson3_helper:get_path_value([{1, binary:list_to_bin(Key)}], Json).

get_grid(Height, Width, Collisions) ->
    Grid = [{Y,X}||Y<-lists:seq(0,Height-1), X<-lists:seq(0,Width-1)],
    GridTiled = lists:zip(Grid, lists:seq(0,length(Grid)-1)),
    GridTree = gb_trees:from_orddict(GridTiled),
    CollisionTree = gb_trees:from_orddict(lists:zip(Collisions, Collisions)),
    gb_trees:map(set_array(CollisionTree), GridTree).

set_array(Collisions) ->
    fun(_Key, TileId) ->
            case gb_trees:lookup(TileId, Collisions) of
                none -> {TileId, 0};
                _ ->
                    {TileId, 1}
            end
    end.

do_is_colliding(X, Y, Grid) ->    
    {value, {_TileID, CollisionBit}} = gb_trees:lookup({X,Y}, Grid),
    CollisionBit == 1.

get_checkpoint(CP) ->
    [Id,X,Y,W,H] = [get_json_value(A, CP) || A <- ["id","x","y","w","h"]],
    #cp{id=Id,x=X,y=Y,w=W,h=H}.

get_mobarea(RoamingArea) ->
    [Id,X,Y,W,H,Type,Nb] = [get_json_value(A, RoamingArea) ||
                       A <- ["id","x","y","width","height","type","nb"]],
    #mobarea{id=Id,x=X,y=Y,w=W,h=H,type=Type,nb=Nb}.

do_is_out_of_bounds(X, Y, PL) ->
    Height = proplists:get_value("height", PL),
    Width = proplists:get_value("width", PL),
    (X < 1) or (X >= Width) or (Y < 1) or (Y >= Height).    

tileid_to_pos(TileId, TileToPos) ->
    {value, Pos} = gb_trees:lookup(TileId, TileToPos),
    Pos.

grid_to_tileid_lookup(Grid) ->
    SwitchKv = fun({Pos, {TileId, _CollisionBit}}) -> {TileId, Pos} end,
    gb_trees:from_orddict(lists:map(SwitchKv, gb_trees:to_list(Grid))).

    

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

-record(map, {json, attributes}).
-record(cp, {id, x, y, w, h}).
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

get_random_starting_position() ->
    gen_server:call(?SERVER, get_random_starting_pos).

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

    CollisionGrid = 
        get_collision_grid(Height, Width, get_json_value("collisions", Json)),
    
    Checkpoints = lists:map(fun get_checkpoint/1,
                            get_json_value("checkpoints", Json)),

    %% Change this to a subset of checkpoints? Mysterious .s field
    %% in cp in map.js.
    StartingAreas = Checkpoints,
                            
    PropList = [{"width", Width}, {"height", Height},
                {"zoneWidth", ZoneWidth}, {"zoneHeight", ZoneHeight},
                {"groupWidth", trunc(Width / ZoneWidth)},
                {"groupHeight", trunc(Height / ZoneHeight)},
                {"collisionGrid", CollisionGrid},
                {"startingAreas", StartingAreas},
                {"checkpoints", Checkpoints}
               ],
    
    Map = #map{json = Json, attributes = PropList},
    {ok, Map}.

handle_call({get_attribute, Attribute}, _From, Map) ->
    {reply, do_get_attribute(Attribute, Map), Map};
handle_call({is_colliding, X, Y}, _From, #map{attributes = PL} = Map) ->
    Grid = proplists:get_value("collisionGrid", PL),
    {reply, do_is_colliding(X, Y, Grid), Map};
handle_call({is_out_of_bounds, X, Y}, _From, #map{attributes = PL} = Map) ->
    {reply, do_is_out_of_bounds(X, Y, PL), Map};
handle_call(get_random_starting_pos, _From, #map{attributes = PL} = Map) ->
    {reply, do_get_random_starting_pos(PL), Map};    
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

get_collision_grid(Height, Width, Collisions) ->
    HeightArray = array:new([{size,Height},{fixed,true},{default,0}]),
    GridArray = array:new([{size,Width},{fixed,true},{default,HeightArray}]),
    Grid = [{Y,X}||Y<-lists:seq(0,Height-1), X<-lists:seq(0,Width-1)],
    GridTiled = lists:zip(Grid, lists:seq(0,length(Grid)-1)),
    lists:foldl(set_array(Collisions), GridArray, GridTiled).

set_array(Collisions) ->
    fun (Element, Array) -> set_array_element(Element, Array, Collisions) end.

set_array_element({{Y,X}, TileIndex}, Array, Collisions) ->
    Value = case lists:member(TileIndex, Collisions) of true -> 1; _ -> 0 end,
    HeightArray = array:get(X, Array),
    NewHeightArray = array:set(Y, Value, HeightArray),
    array:set(X, NewHeightArray, Array).

do_is_colliding(X, Y, Grid) ->    
    array:get(Y - 1, array:get(X - 1, Grid)) == 1.

get_checkpoint(CP) ->
    [Id,X,Y,W,H] = [get_json_value(A, CP) || A <- ["id","x","y","w","h"]],
    #cp{id=Id,x=X,y=Y,w=W,h=H}.

do_get_random_starting_pos(PL) ->
    StartingAreas = proplists:get_value("startingAreas", PL),
    random:seed(erlang:now()),
    Id = random:uniform(length(StartingAreas)),
    #cp{x = X, y = Y} = lists:keyfind(Id, #cp.id, StartingAreas),
    F = fun(X1,Y1) -> do_is_out_of_bounds(X1, Y1, PL) end,
    get_valid(X, Y, false, F).

get_valid(X, Y, true, _) -> {X, Y};
get_valid(X, Y, _, F) ->
    Op = fun() -> case random:uniform(2) of 1 -> '+'; _ -> '+' end end,
    Xn = erlang:(Op())(X,random:uniform(3)),
    Yn = erlang:(Op())(Y,random:uniform(3)),
    get_valid(Xn, Yn, F(Xn, Yn), F).
    
do_is_out_of_bounds(X, Y, PL) ->
    Height = proplists:get_value("height", PL),
    Width = proplists:get_value("width", PL),
    (X < 1) or (X >= Width) or (Y < 1) or (Y >= Height).    



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

    PropList = [{"width", Width}, {"height", Height},
                {"zoneWidth", ZoneWidth}, {"zoneHeight", ZoneHeight},
                {"groupWidth", trunc(Width / ZoneWidth)},
                {"groupHeight", trunc(Height / ZoneHeight)},
                {"collisionGrid", CollisionGrid}],
    
    Map = #map{json = Json, attributes = PropList},
    {ok, Map}.

handle_call({get_attribute, Attribute}, _From, Map) ->
    {reply, do_get_attribute(Attribute, Map), Map};
handle_call({is_colliding, X, Y}, _From, #map{attributes = PL} = Map) ->
    Grid = proplists:get_value("collisionGrid", PL),
    {reply, do_is_colliding(X, Y, Grid), Map};
handle_call({is_out_of_bounds, X, Y}, _From, #map{attributes = PL} = Map) ->
    Height = proplists:get_value("height", PL),
    Width = proplists:get_value("width", PL),
    io:format("~p~n", [{X,Width,Y,Height}]),
    Reply = (X < 1) or (X >= Width) or (Y < 1) or (Y >= Height),
    {reply, Reply, Map};
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

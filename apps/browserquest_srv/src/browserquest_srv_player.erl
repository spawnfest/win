%%%-------------------------------------------------------------------
%%% @author Gustav Simonsson  <gustav.simonsson@gmail.com>
%%% @doc
%%% Browserquest server template
%%% @end
%%% Created : 7 July 2012 by <gustav.simonsson@gmail.com>
%%%-------------------------------------------------------------------
-module(browserquest_srv_player).

-behaviour(gen_server).

%% API
-export([
	 start_link/3,
	 get_status/1,
	 move/3,
	 set_checkpoint/2,
	 update_zone/1,
	 get_zone/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-compile(export_all).

-define(SERVER, ?MODULE). 
-define(CALC_HP(ArmorLevel), 80 + ((ArmorLevel - 1) * 30)).
-define(APP, browserquest_srv).

-record(player, {
	  id,
	  name,
	  armor,
	  weapon,
	  hitpoints,
	  pos_x,
	  pos_y,
	  checkpoint,
	  zone
	 }).

-record(state, {
	  riak :: pid(), 
	  player :: #player{},
	  last_view
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

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([Name, Armor, Weapon]) ->
    Id = generate_id(),
    Hitpoints = ?CALC_HP(Armor),
    PosX = 10,
    PosY = 10,
    
    {ok, RiakIP} = application:get_env(?APP, riak_ip),
    {ok, RiakPort} = application:get_env(?APP, riak_port),

    {ok, RiakPid} = riakc_pb_socket:start_link(RiakIP, RiakPort), 

    {ok, #state{
       riak = RiakPid,
       player = #player{
	 id = Id,
	 name = Name,
	 armor = Armor,
	 weapon = Weapon,
	 pos_x = PosX,
	 pos_y = PosY,
	 hitpoints = Hitpoints,
	 checkpoint = 0,
	 zone = make_zone(PosX, PosY)
	}
      }}.

handle_call({get_status}, _From, State = #state{riak = Riak, player = #player{id = Id, name = Name, zone = Zone, pos_x = X, pos_y = Y, hitpoints = HP}}) ->
    add_action(Riak, Zone, Id, move, [X, Y]),
    {reply, {ok, [Id, Name, X, Y, HP]}, State};

handle_call({move, X, Y}, _From, State = #state{riak = Riak, player = Player}) ->
    NewState = State#state{player = Player#player{pos_x = X, pos_y = Y}},
    add_action(Riak, Player#player.zone, Player#player.id, move, [X, Y]),
    {reply, {ok, [Player#player.id, X, Y]}, NewState};

handle_call({set_checkpoint, Value}, _From, State = #state{player = Player}) ->
    {reply, ok, State#state{player = Player#player{checkpoint = Value}}};

handle_call({update_zone}, _From, State = #state{player = Player}) ->
    %% Delete old zone and insert the new one
    Zone = make_zone(Player#player.pos_x, Player#player.pos_y),
    move_to_zone(Zone, State),
    {reply, ok, State#state{player = Player#player{zone = Zone}}};

handle_call({get_zone}, _From, State = #state{player = Player}) ->
    {reply, {ok, Player#player.zone}, State};

handle_call({get_surrondings}, _From, State) ->
    {reply, ok, State};

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
generate_id() ->
    random:seed(erlang:now()),
    random:uniform(1000000).

add_action(Riak, Zone, Id, Action, Args) ->
    store_in_riak(Riak, Zone, Id, [Action, Args]).

move_to_zone(NewZone, #state{riak = Riak, player = Player}) ->
    lager:debug("Moving out of zone"),
    ok = delete_in_riak(Riak, Player#player.zone, Player#player.id), 
    add_action(Riak, NewZone, Player#player.id, move, [Player#player.pos_x, Player#player.pos_y]).
    
make_zone(PosX, PosY) ->
    Zone = erlang:trunc(PosX/(PosX rem 28))*erlang:trunc(PosY/(PosY rem 11)),
    ZoneString = erlang:integer_to_list(Zone),
    ensure_bin("ZONE"++ZoneString).

store_in_riak(Riak, Zone, Id, Term) ->
    Object = riakc_obj:new(Zone, ensure_bin(Id), <<>>),
    Object2 = encode_term(Object, Term),
    riakc_pb_socket:put(Riak, Object2).

delete_in_riak(Riak, Zone, Id) ->
    riakc_pb_socket:delete(Riak, Zone, ensure_bin(Id)).

decode_term(Object) ->
  case riakc_obj:get_content_type(Object) of
    <<"application/x-erlang-term">> ->
      try
        {ok, binary_to_term(riakc_obj:get_value(Object))}
      catch
        _:Reason ->
          {error, Reason}
      end;
    Ctype ->
      {error, {unknown_ctype, Ctype}}
  end.

encode_term(Object, Term) ->
  riakc_obj:update_value(Object, term_to_binary(Term, [compressed]),
  <<"application/x-erlang-term">>).


ensure_bin(Int) when is_integer(Int) ->
    ensure_bin(erlang:integer_to_list(Int));
ensure_bin(List) when is_list(List) ->
    erlang:list_to_binary(List);
ensure_bin(Bin) when is_binary(Bin) ->
    Bin.

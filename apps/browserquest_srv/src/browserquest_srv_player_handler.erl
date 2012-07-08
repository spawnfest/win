%%%-------------------------------------------------------------------
%%% @author Niclas Axelsson <burbas@Niclass-MacBook-Pro.local>
%%% @copyright (C) 2012, Niclas Axelsson
%%% @doc
%%%
%%% @end
%%% Created :  8 Jul 2012 by Niclas Axelsson <burbas@Niclass-MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(browserquest_srv_player_handler).

-behaviour(gen_server).

%% API
-export([
	 start_link/0,
	 register/2,
	 unregister/1,
	 event/2,
	 move_zone/2
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {
	  zones :: dict()
	 }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

register(Zone, SpawnInfo) ->
    Pid = self(),
    gen_server:call(?MODULE, {register, Pid, Zone}),
    event(Zone, SpawnInfo).

unregister(Zone) ->
    Pid = self(),
    gen_server:call(?MODULE, {unregister, Pid, Zone}).

event(Zone, Message) ->
    Pid = self(),
    gen_server:call(?MODULE, {event, Pid, Zone, Message}).

move_zone(OldZone, NewZone) ->
    Pid = self(),
    gen_server:call(?MODULE, {unregister, Pid, OldZone}),
    gen_server:call(?MODULE, {register, Pid, NewZone}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{zones = dict:new()}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({register, Pid, Zone}, _From, State = #state{zones = Zones}) ->
    UpdatedZones = dict:update(Zone, fun(Nodes) -> 
					     [Pid|Nodes] 
				     end, [Pid], Zones),
    {reply, ok, State#state{zones = UpdatedZones}};

handle_call({unregister, Pid, Zone}, _From, State = #state{zones = Zones}) ->
    UpdatedZones = dict:update(Zone, fun(Nodes) ->
					     [ X || X <- Nodes, X /= Pid ]
				     end, [], Zones),
    {reply, ok, State#state{zones = UpdatedZones}};

handle_call({event, Pid, Zone, Message}, _From, State = #state{zones = Zones}) ->
    case dict:find(Zone, Zones) of
	{ok, Nodes} ->
	    [ gen_server:cast(Node, {event, Pid, Message}) || Node <- Nodes, Node /= Pid ];
	_ ->
	    []
    end,
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
decode_term(Object) ->
  case riakc_obj:get_content_type(Object) of
    "application/x-erlang-term" ->
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

%%%===================================================================
%%% Exported functions
%%%===================================================================
make_zone(PosX, PosY) ->
    Zone = erlang:trunc(PosX/(PosX rem 28))*erlang:trunc(PosY/(PosY rem 11)),
    ZoneString = erlang:integer_to_list(Zone),
    ensure_bin("ZONE"++ZoneString).

%%%-------------------------------------------------------------------
%%% @author Gustav Simonsson  <gustav.simonsson@gmail.com>
%%% @doc
%%% Cowboy handler.
%%% @end
%%% Created : 7 July 2012 by <gustav.simonsson@gmail.com>
%%%-------------------------------------------------------------------
-module(browserquest_srv_handler).
-compile(export_all).

-behaviour(cowboy_http_handler).  
-behaviour(cowboy_http_websocket_handler). 

-include("../include/browserquest.hrl").

% Behaviour cowboy_http_handler  
-export([init/3, handle/2, terminate/2]).  
  
% Behaviour cowboy_http_websocket_handler  
-export([  
    websocket_init/3, websocket_handle/3,  
    websocket_info/3, websocket_terminate/3  
]).  

-define(APP, browserquest_srv).

-record(state, {
	  riak,
	  player,
	  tick_time
	 }).

%%%===================================================================
%%% API
%%%===================================================================
% Called to know how to dispatch a new connection.  
init({tcp, http}, Req, _Opts) ->  
    lager:debug("Request: ~p", [Req]),  
    % "upgrade" every request to websocket,  
    % we're not interested in serving any other content.
    {upgrade, protocol, cowboy_http_websocket}.

% Should never get here.  
handle(Req, State) ->  
    lager:debug("Unexpected request: ~p", [Req]),  
    {ok, Req2} = cowboy_http_req:reply(404, [  
        {'Content-Type', <<"text/html">>}  
    ]),  
    {ok, Req2, State}.
  
terminate(_Req, _State) ->  
    ok.
  
% Called for every new websocket connection.  
websocket_init(tcp, Req, []) ->  
    lager:debug("New client"),
    
    Req2 = cowboy_http_req:compact(Req),

    self() ! <<"Send gogo">>,

    {ok, TickTime} = application:get_env(?APP, tick_time),

    spawn(?MODULE, make_tick, [self(), TickTime]),

    {ok, Req2, #state{tick_time = TickTime}}.  

websocket_handle({text, Msg}, Req, State) ->  
    Args = mochijson3:decode(Msg),
    {Type, Reply, NewState} = parse_action(Args, State),
    
    case Type of
	json ->
	    self() ! {json, Reply}, Req, State,
	    {ok, Req, NewState};
	_ ->
	    {ok, Req, NewState}
    end;
  
% With this callback we can handle other kind of  
% messages, like binary.  
websocket_handle(_Any, Req, State) ->  
    browserquest_srv_util:unexpected_info(
      ?MODULE,"websocket binary received", State),
    {ok, Req, State}.  


% Called when a text message arrives. 
websocket_info(<<"Send gogo">>, Req, State) ->
    lager:debug("Sending 'go' message to client"),
    {reply, {text, <<"go">>}, Req, State};

websocket_info(<<"tick">>, Req, State = #state{player = undefined}) ->
    {ok, Req, State};
websocket_info(<<"tick">>, Req, State = #state{player = Player}) ->
    case browserquest_srv_player:get_surrondings(Player) of
	[] ->
	    ok;
	ActionList ->
	    self() ! {json, ActionList}
    end,
    {ok, Req, State};

websocket_info({json, Message}, Req, State) ->
    Json = mochijson3:encode(Message),
    {reply, {text, Json}, Req, State};
  
websocket_info(Msg, Req, State) ->
    lager:debug("Got unknown message: ~p", [Msg]),
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, #state{player = Player}) ->      
    lager:debug("Connection closed"),
    browserquest_srv_player:stop(Player),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
parse_action([?HELLO, Name, Armor, Weapon], State) ->
    %% This is a player call
    {ok, Player} = browserquest_srv_player:start_link(Name, Armor, Weapon),
    {ok, Status} = browserquest_srv_player:get_status(Player),
    {json, [?WELCOME|Status], State#state{player = Player}};

parse_action([?MOVE, X, Y], State = #state{player = Player}) ->
    {ok, Status} = browserquest_srv_player:move(Player, X, Y),
    {json, [?MOVE|Status], State};

parse_action([?CHAT, Message], State = #state{player = Player}) ->
    {ok, Return} = browserquest_srv_player:chat(Player, Message),
    {json, Return, State};

parse_action([?TELEPORT, X, Y], State = #state{player = Player}) ->
    {ok, Status} = browserquest_srv_player:move(Player, X, Y),
    {json, [?TELEPORT|Status], State};

parse_action([?CHECK, Value], State = #state{player = Player}) ->
    browserquest_srv_player:set_checkpoint(Player, Value),
    {ok, [], State};

parse_action([?ZONE], State = #state{player = Player}) ->
    browserquest_srv_player:update_zone(Player),
    {ok, [], State};

parse_action(ActionList, _State) ->
    lager:error("Faulty actionlist: ~p", [ActionList]),
    exit({faulty_actionlist, ActionList}).

make_tick(Node, TickTime) ->
    Node ! <<"tick">>,
    receive 
	stop ->
	    ok
    after 
	TickTime ->
	    make_tick(Node, TickTime)
    end.


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

% Behaviour cowboy_http_handler  
-export([init/3, handle/2, terminate/2]).  
  
% Behaviour cowboy_http_websocket_handler  
-export([  
    websocket_init/3, websocket_handle/3,  
    websocket_info/3, websocket_terminate/3  
]).  

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
    lager:debug("New client: ~p", [Req]),  
    Req2 = cowboy_http_req:compact(Req),  
    self() ! <<"Send gogo">>,
    {ok, Req2, undefined}.  
  
websocket_handle({text, Msg}, Req, State) ->  
    Args = mochijson3:decode(Msg),
    parse_action(Args),
    lager:debug("Received: ~p", [Msg]),  
    {reply,  
        {text, << "Responding to ", Msg/binary >>},  
        Req, State, hibernate  
    };
  
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

% Other messages from the system are handled here.  
websocket_info(_Info, Req, State) ->
    browserquest_srv_util:unexpected_info(?MODULE, _Info, State),
    {ok, Req, State, hibernate}.  
  
websocket_terminate(_Reason, _Req, _State) ->  
    ok.  

%%%===================================================================
%%% Internal funxggctions
%%%===================================================================
parse_action([?HELLO, Name, X, Y]) ->
    %% This is a player call
    ok;
parse_action(ActionList) ->
    lager:error("Faulty actionlist: ~p", [ActionList]),
    exit({faulty_actionlist, ActionList}).


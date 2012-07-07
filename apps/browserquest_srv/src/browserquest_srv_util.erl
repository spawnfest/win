%%%-------------------------------------------------------------------
%%% @author Gustav Simonsson <gustav.simonsson@gmail.com>
%%% @doc
%%% 
%%% @end
%%% Created : 7 July 2012 by <gustav.simonsson@gmail.com>
%%%-------------------------------------------------------------------
-module(browserquest_srv_util).
-compile(export_all).

unexpected_call(Module, Request, From, State) ->
    lager:warning("[~p] Unexpected call, Request, From, State: ~p~n",
                  [Module, {Request, From, State}]).

unexpected_cast(Module, Msg, State) ->
    lager:warning("[~p] Unexpected cast, Msg, State: ~p~n",
                  [Module, {Msg, State}]).

unexpected_info(Module, Info, State) ->
    lager:warning("[~p] Unexpected info, Info, State: ~p~n",
                  [Module, {Info, State}]).

%%%-------------------------------------------------------------------
%%% @author Gustav Simonsson <gustav.simonsson@gmail.com>
%%% @doc
%%% 
%%% @end
%%% Created : 7 July 2012 by <gustav.simonsson@gmail.com>
%%%-------------------------------------------------------------------
-module(browserquest_srv_util).

-include("../include/browserquest.hrl").

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

type_to_internal(<<"rat">>) -> ?RAT;
type_to_internal(<<"skeleton">>) -> ?SKELETON;
type_to_internal(<<"goblin">>) -> ?GOBLIN;
type_to_internal(<<"ogre">>) -> ?OGRE;
type_to_internal(<<"spectre">>) -> ?SPECTRE;
type_to_internal(<<"crab">>) -> ?CRAB;
type_to_internal(<<"bat">>) -> ?BAT;
type_to_internal(<<"wizard">>) -> ?WIZARD;
type_to_internal(<<"eye">>) -> ?EYE;
type_to_internal(<<"snake">>) -> ?SNAKE;
type_to_internal(<<"skeleton2">>) -> ?SKELETON2;
type_to_internal(<<"boss">>) -> ?BOSS;
type_to_internal(<<"deathknight">>) -> ?DEATHKNIGHT;
type_to_internal(Unmapped) -> Unmapped.

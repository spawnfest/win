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
type_to_internal(<<"firefox">>) -> ?FIREFOX;
type_to_internal(<<"clotharmor">>) -> ?CLOTHARMOR;
type_to_internal(<<"leatherarmor">>) -> ?LEATHERARMOR;
type_to_internal(<<"mailarmor">>) -> ?MAILARMOR;
type_to_internal(<<"platearmor">>) -> ?PLATEARMOR;
type_to_internal(<<"redarmor">>) -> ?REDARMOR;
type_to_internal(<<"goldenarmor">>) -> ?GOLDENARMOR;
type_to_internal(<<"flask">>) -> ?FLASK;
type_to_internal(<<"burger">>) -> ?BURGER;
type_to_internal(<<"chest">>) -> ?CHEST;
type_to_internal(<<"firepotion">>) -> ?FIREPOTION;
type_to_internal(<<"cake">>) -> ?CAKE;
type_to_internal(<<"guard">>) -> ?GUARD;
type_to_internal(<<"king">>) -> ?KING;
type_to_internal(<<"octocat">>) -> ?OCTOCAT;
type_to_internal(<<"villagegirl">>) -> ?VILLAGEGIRL;
type_to_internal(<<"villager">>) -> ?VILLAGER;
type_to_internal(<<"priest">>) -> ?PRIEST;
type_to_internal(<<"scientist">>) -> ?SCIENTIST;
type_to_internal(<<"agent">>) -> ?AGENT;
type_to_internal(<<"rick">>) -> ?RICK;
type_to_internal(<<"nyan">>) -> ?NYAN;
type_to_internal(<<"sorcerer">>) -> ?SORCERER;
type_to_internal(<<"beachnpc">>) -> ?BEACHNPC;
type_to_internal(<<"forestnpc">>) -> ?FORESTNPC;
type_to_internal(<<"desertnpc">>) -> ?DESERTNPC;
type_to_internal(<<"lavanpc">>) -> ?LAVANPC;
type_to_internal(<<"coder">>) -> ?CODER;
type_to_internal(<<"sword1">>) -> ?SWORD1;
type_to_internal(<<"sword2">>) -> ?SWORD2;
type_to_internal(<<"redsword">>) -> ?REDSWORD;
type_to_internal(<<"goldensword">>) -> ?GOLDENSWORD;
type_to_internal(<<"morningstar">>) -> ?MORNINGSTAR;
type_to_internal(<<"axe">>) -> ?AXE;
type_to_internal(<<"bluesword">>) -> ?BLUESWORD;
type_to_internal(Unmapped) -> Unmapped.

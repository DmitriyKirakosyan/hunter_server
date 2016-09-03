%%%-------------------------------------------------------------------
%%% @author dima
%%% @copyright (C) 2016, nooneknows
%%% @doc
%%%
%%% @end
%%% Created : 03. Sep 2016 7:50 AM
%%%-------------------------------------------------------------------
-module(hunter_simple_bot).
-author("dima").

-include("hunter_config.hrl").

%% API
-export([tick/2]).


-spec tick(#game_state{}, number()) -> {#game_state{}, list()}.
tick(GameState, _TimeDelta) ->
    {GameState, []}.

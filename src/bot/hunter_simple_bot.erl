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


-spec tick_all(list(), #game_state{}, number()) -> {#game_state{}, list()}.
tick_all([#player{is_bot = true} = Bot | Players], GameState, TimeDelta) ->
    {UpdatedState, Actions} = tick(Bot, GameState, TimeDelta),
    {FinalState, RestActions} = tick_all(Players, UpdatedState, TimeDelta),
    {FinalState, [Actions ++ RestActions]};
tick_all([_ | Players], GameState, TimeDelta) ->
    tick_all(Players, GameState, TimeDelta).

-spec tick(#game_state{}, number()) -> {#game_state{}, list()}.
tick(#game_state{players = Players} = GameState, TimeDelta) ->
    tick_all(Players, GameState, TimeDelta).

-spec tick(#player{}, #game_state{}, number()) -> {#game_state{}, list()}.
tick(Bot, GameState, TimeDelta) ->
    {GameState, compute_actions(Bot, GameState#game_state.players, TimeDelta)}.

-spec compute_actions(#player{}, list(), number()) -> list().
compute_actions(Bot, Players, TimeDelta) ->
    consider_move_action(TimeDelta) ++ consider_shoot_action(Bot, Players, TimeDelta).

consider_move_action(TimeDelta) ->
    MoveRandom = rand:uniform(trunc(100 / TimeDelta) + 1),
    if MoveRandom < 10 ->
            {X, Y} = hunter_utils:random_map_point(),
            [?MAKE_MOVE_ACTION(X, Y)];
        true -> []
    end.

consider_shoot_action(Bot, [Bot | Players], TimeDelta) ->
    consider_shoot_action(Bot, Players, TimeDelta);
consider_shoot_action(_, [#player{id = PlayerId} | _], TimeDelta) ->
    ShootRandom = rand:uniform(trunc(100 / TimeDelta) + 1),
    if
        ShootRandom < 10 -> [?MAKE_SHOOT_ACTION(PlayerId)];
        true -> []
    end.



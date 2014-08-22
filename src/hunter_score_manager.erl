-module (hunter_score_manager).

-export ([get_result_action/2]).

-include ("hunter_config.hrl").

get_result_action(Results, Players) ->
    [
        [
            {name, Player#player.name},
            {kills, get_kills_for_player(Player#player.id, Results)},
            {deaths, get_deaths_for_player(Player#player.id, Results)}
        ] 

    || Player <- Players].


get_kills_for_player(PlayerId, Results) ->
    lists:foldl(
        fun(Action, Acc) ->
            KillerPlayerId = proplists:get_value(killer, Action),
            if
                PlayerId =:= KillerPlayerId->
                    Acc + 1;
                true ->
                    Acc
            end
        end
    , 0, Results).

get_deaths_for_player(PlayerId, Results) ->
    lists:foldl(
        fun(Action, Acc) ->
            ActionType = proplists:get_value(action, Action),
            ActionPlayerId = proplists:get_value(id, Action),
            if
                PlayerId =:= ActionPlayerId andalso ActionType =:= ?DEAD_ACTION ->
                    Acc + 1;
                true ->
                    Acc
            end
        end
    , 0, Results).

-module (hunter_debug_util).

-export ([update/4]).

-include ("hunter_config.hrl").


update(ActionType, DiffStones, TimeDelta, Debug) when TimeDelta < 0 ->
    TimeDeltaSigned = Debug#debug.time_delta_signed,
    count_stones(ActionType, DiffStones, Debug#debug{time_delta_signed=TimeDeltaSigned+1});
update(ActionType, DiffStones, TimeDelta, Debug) ->
    count_stones(ActionType, DiffStones, Debug).

count_stones(ActionType, DiffStones, Debug) ->
    NewDebug = if
        ActionType =:= ?PICK_ACTION ->
            Debug#debug{pick_num=Debug#debug.pick_num+1};
        true -> Debug
    end,
    update_debug(DiffStones, NewDebug).


update_debug([], Debug) -> Debug;
update_debug([Action | Actions],
                    #debug{added_num=AddedNum, removed_num=RemovedNum} = Debug) ->
    ActionType = proplists:get_value(action, Action),
    case ActionType of
        ?STONE_ADDED_ACTION ->
            update_debug(Actions, Debug#debug{added_num=AddedNum+1});
        ?STONE_REMOVED_ACTION ->
            update_debug(Actions, Debug#debug{removed_num=RemovedNum+1});
        _Else ->
            update_debug(Actions, Debug)
    end.

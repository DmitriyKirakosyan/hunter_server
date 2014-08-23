-module (hunter_debug_util).

-export ([check_time/2, inc_pick/1, count_stones/2]).

-include ("hunter_config.hrl").


check_time(TimeDelta, Debug) when TimeDelta < 0 ->
    TimeDeltaSigned = Debug#debug.time_delta_signed,
    Debug#debug{time_delta_signed=TimeDeltaSigned+1};
check_time(_TimeDelta, Debug) ->
    Debug.

inc_pick(Debug) ->
    Debug#debug{pick_num=Debug#debug.pick_num+1}.

count_stones([], Debug) -> Debug;
count_stones([Action | Actions],
                    #debug{added_num=AddedNum, removed_num=RemovedNum} = Debug) ->
    ActionType = proplists:get_value(action, Action),
    case ActionType of
        ?STONE_ADDED_ACTION ->
            count_stones(Actions, Debug#debug{added_num=AddedNum+1});
        ?STONE_REMOVED_ACTION ->
            count_stones(Actions, Debug#debug{removed_num=RemovedNum+1});
        _Else ->
            count_stones(Actions, Debug)
    end.

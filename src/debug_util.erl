-module (debug_util).

-export ([count_stones/3]).

-include ("hunter_config.hrl").

count_stones(ActionType, DiffStones, StonesCounter) ->
    NewStonesCounter = if
        ActionType =:= ?PICK_ACTION ->
            StonesCounter#stones_counter{pick_num=StonesCounter#stones_counter.pick_num+1};
        true -> StonesCounter
    end,
    update_stones_counter(DiffStones, NewStonesCounter).


update_stones_counter([], StonesCounter) -> StonesCounter;
update_stones_counter([Action | Actions],
                    #stones_counter{added_num=AddedNum, removed_num=RemovedNum} = StonesCounter) ->
    ActionType = proplists:get_value(action, Action),
    case ActionType of
        ?STONE_ADDED_ACTION ->
            update_stones_counter(Actions, StonesCounter#stones_counter{added_num=AddedNum+1});
        ?STONE_REMOVED_ACTION ->
            update_stones_counter(Actions, StonesCounter#stones_counter{removed_num=RemovedNum+1});
        _Else ->
            update_stones_counter(Actions, StonesCounter)
    end.

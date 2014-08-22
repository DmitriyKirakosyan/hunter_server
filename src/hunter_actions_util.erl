-module (hunter_actions_util).

-export ([remove_actions/2, remove_actions_except_first/2]).


remove_actions([], Actions) -> Actions;
remove_actions([ActionType | ActionTypes], Actions) ->
    NewActions = remove_actions(ActionType, Actions),
    remove_actions(ActionTypes, NewActions);

remove_actions(ActionType, Actions) ->
    lists:filter(
        fun(Action) ->
            ActionItemType = proplists:get_value(action, Action),
            ActionType =/= ActionItemType
        end
    , Actions).

remove_actions_except_first(ActionType, Actions) -> remove_actions_except_first(ActionType, Actions, false).


remove_actions_except_first(_, [], _) -> [];
remove_actions_except_first(ActionType, [Action | Actions], WasMet) ->
    ActionItemType = proplists:get_value(action, Action),
    if
        ActionType =:= ActionItemType andalso WasMet =:= true ->
            remove_actions_except_first(ActionType, Actions, true);
        true ->
            [Action | remove_actions_except_first(ActionType, Actions, WasMet)]
    end.
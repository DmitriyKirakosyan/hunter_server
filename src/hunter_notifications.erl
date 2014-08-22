-module (hunter_notifications).

-export ([calc_notifications/3]).


-include ("hunter_config.hrl").

calc_notifications(Player, ?LOGIN_ACTION, {_Players, Stones}) ->
    ActualStones = hunter_stone_manager:get_actual_stones(Stones),
    StoneActions = [[{action, ?STONE_ADDED_ACTION}, {x, Stone#stone.x}, {y, Stone#stone.y}]
                    || Stone <- ActualStones],
    io:format("stones data to send : ~p~n", [StoneActions]),
    StoneActionTypes = [?STONE_ADDED_ACTION, ?STONE_REMOVED_ACTION],
    UpdatedNotifications = hunter_actions_util:remove_actions(StoneActionTypes, Player#player.notifications),
    CombinedNotifications = lists:concat([UpdatedNotifications, StoneActions]),
    calc_notifications(Player#player{notifications = CombinedNotifications});
calc_notifications(Player, _, _) ->
    calc_notifications(Player).

calc_notifications(Player) ->
    hunter_actions_util:remove_actions_except_first(?MOVE_ACTION, Player#player.notifications).

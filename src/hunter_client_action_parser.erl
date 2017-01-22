-module(hunter_client_action_parser).
-author("dima").

-include("hunter_client_actions.hrl").

%% API
-export([parse/1]).

parse(Action) ->
    ActionName = proplists:get_value(action, Action),
    parse(ActionName, Action).

parse(?LOGIN_ACTION, LoginAction) ->
    #client_action_login{
        action_name = ?LOGIN_ACTION,
        player_id = proplists:get_value(id, LoginAction),
        player_name = proplists:get_value(name, LoginAction)
    };

parse(?DEAD_ACTION, _) ->
    #client_action_basic{ action_name = ?DEAD_ACTION };

parse(?PING_ACTION, _) ->
    #client_action_basic{ action_name = ?PING_ACTION };

parse(?LOGOUT_ACTION, _) ->
    #client_action_basic{ action_name = ?LOGOUT_ACTION };

parse(?PICK_WEAPON_ACTION, Action) ->
    #client_action_pickWeapon{
        action_name = ?PICK_WEAPON_ACTION,
        position = #point{
            x = proplists:get_value(x, Action),
            y = proplists:get_value(y, Action)
        }
    };

parse(?PICK_BONUS_ACTION, Action) ->
    #client_action_pickWeapon{
        action_name = ?PICK_BONUS_ACTION,
        position = #point{
            x = proplists:get_value(x, Action),
            y = proplists:get_value(y, Action)
        }
    };


parse(_, _) -> undefined.
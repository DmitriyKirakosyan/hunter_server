-author("dima").

-include("hunter_config.hrl").

-record(client_action_basic, {
    action_name :: binary()
}).

-record(client_action_login, {
    action_name :: binary(),
    player_id :: binary(),
    player_name :: binary(),
    is_obs :: boolean()
}).

-record(client_action_pickBonus, {
    action_name :: binary(),
    position :: #point{}
}).

-record(client_action_pickWeapon, {
    action_name :: binary(),
    position :: #point{}
}).
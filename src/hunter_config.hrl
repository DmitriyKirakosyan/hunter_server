-define (STONE_ADDED_ACTION, <<"stone_added">>).
-define (STONE_REMOVED_ACTION, <<"stone_removed">>).
-define (BONUS_ADDED_ACTION, <<"bonus_added">>).
-define (BONUS_REMOVED_ACTION, <<"bonus_removed">>).
-define (TIME_ACTION, <<"time">>).
-define (LOGOUT_ACTION, <<"logout">>).
-define (LOGIN_ACTION, <<"login">>).
-define (PING_ACTION, <<"ping">>).
-define (PICK_WEAPON_ACTION, <<"pick_weapon">>).
-define (PICK_BONUS_ACTION, <<"pick_bonus">>).
-define (MOVE_ACTION, <<"move">>).
-define (DEAD_ACTION, <<"dead">>).
-define (RESULT_ACTION, <<"result">>).

%% bonus
-define (BONUS_HEAL, <<"bonus_heal">>).
-define (BONUS_SPEED, <<"bonus_speed">>).

-define (BONUSES_IN_ROW, 20).

-define (STONES_IN_ROW, 3).
-define (PLAY_TIME, 20).

-define (MAKE_TIME_ACTION(Time), [{action, ?TIME_ACTION}, {time_left, Time}]).

-define (ACTION_TOKEN, <<"#">>).

-define (MAP_WIDTH, 1350).
-define (MAP_HEIGHT, 1350).


-record (player, {
    id,
    name,
    notifications = [],
    last_message
}).

-record (notification, {
    id,
    action,
    x,
    y
}).

-record (stone, {
    x,
    y,
    appearing_time=0
}).

-record (bonus, {
    x,
    y,
    type,
    duration=0,
    appearing_time=0
    }).

%% debug
-record (debug, {
    pick_num = 0,
    added_num = 0,
    removed_num = 0,
    time_delta_signed = 0
}).

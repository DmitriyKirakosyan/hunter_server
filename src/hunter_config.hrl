-define (STONE_ADDED_ACTION, <<"stone_added">>).
-define (STONE_REMOVED_ACTION, <<"stone_removed">>).
-define (TIME_ACTION, <<"time">>).
-define (LOGOUT_ACTION, <<"logout">>).
-define (LOGIN_ACTION, <<"login">>).
-define (PING_ACTION, <<"ping">>).
-define (PICK_ACTION, <<"pick">>).
-define (MOVE_ACTION, <<"move">>).
-define (DEAD_ACTION, <<"dead">>).
-define (RESULT_ACTION, <<"result">>).

-define (STONES_IN_ROW, 3).
-define (PLAY_TIME, 20).

-define (MAKE_TIME_ACTION(Time), [{action, ?TIME_ACTION}, {time_left, Time}]).

-define (ACTION_TOKEN, <<"#">>).

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

%% debug
-record (debug, {
    pick_num = 0,
    added_num = 0,
    removed_num = 0,
    time_delta_signed = 0
}).

-define (STONE_ADDED_ACTION, <<"stone_added">>).
-define (STONE_REMOVED_ACTION, <<"stone_removed">>).
-define (LOGOUT_ACTION, <<"logout">>).
-define (LOGIN_ACTION, <<"login">>).
-define (PING_ACTION, <<"ping">>).
-define (PICK_ACTION, <<"pick">>).
-define (MOVE_ACTION, <<"move">>).

-record (player, {
    id,
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
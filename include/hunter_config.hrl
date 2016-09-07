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
-define (GAME_INFO, <<"game_info">>).
-define (RESULT_ACTION, <<"result">>).
-define (SHOOT_ACTION, <<"shoot">>).

%% bonus
-define (BONUS_HEAL, <<"bonus_heal">>).
-define (BONUS_SPEED, <<"bonus_speed">>).

-define (BONUSES_IN_ROW, 20).

-define (STONES_IN_ROW, 3).
-define (PLAY_TIME, 20).

-define (MAKE_TIME_ACTION(Time), [{action, ?TIME_ACTION}, {time_left, Time}]).

-define (MAKE_GAME_INFO_ACTION(Players), [{action, ?GAME_INFO}, {players, Players}]).

-define (MAKE_STONE_REMOVED_ACTION(X, Y), [{action, ?STONE_REMOVED_ACTION}, {x, X}, {y, Y}]).
-define (MAKE_STONE_ADDED_ACTION(X, Y), [{action, ?STONE_ADDED_ACTION}, {x, X}, {y, Y}]).

-define (MAKE_MOVE_ACTION(X, Y), [{action, ?MOVE_ACTION}, {x, X}, {y, Y}]).
-define (MAKE_SHOOT_ACTION(PlayerId), [{action, ?SHOOT_ACTION}, {id, PlayerId}]).

-define (ACTION_TOKEN, <<"#">>).

-define (MAP_WIDTH, 1000).
-define (MAP_HEIGHT, 1000).

-define (EMPTY_SERVER_RESPONSE, []).




-record (player, {
    id,
    name,
    notifications = [],
    last_message,
    is_bot :: boolean()
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

-record(bot, {
    id :: atom(),
    name :: binary(),
    last_active_time :: number()
}).

%% debug
-record (debug, {
    pick_num = 0,
    added_num = 0,
    removed_num = 0,
    time_delta_signed = 0,
    ignore_actions = []
}).

-record (game_state, {
    players = [] :: list(),
    stones = [],
    bonuses = [],
    results = [],
    time_left = 0,
    started = false,
    debug = #debug{}
}).


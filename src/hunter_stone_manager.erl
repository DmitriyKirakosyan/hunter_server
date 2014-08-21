-module (hunter_stone_manager).

-export ([create_stones/0, update_stones/1]).

-define (MAP_WIDTH, 1350).
-define (MAP_HEIGHT, 1350).

-define (FIELDS_NUM, 5).

-include ("hunter_config.hrl").


create_stones() ->
    [get_random_stone(I) || I <- lists:seq(0, ?FIELDS_NUM * ?FIELDS_NUM - 1)].

update_stones(Stones) ->
    LastTime = ets:first(last_time),
    MillisecondsNow = milliseconds_now(),
    ets:delete(last_time),
    ets:insert(last_time, {MillisecondsNow}),

    TimeDelta = MillisecondsNow - LastTime / 1000,
    lists:map(
        fun(Stone) ->
            NewStone = if
                Stone#stone.appearing_time < TimeDelta ->
                    Stone#stone{appearing_time=0};
                true -> Stone#stone{appearing_time = Stone#stone.appearing_time - TimeDelta}
            end
        end

    , Stones).

get_random_stone(Position) ->
    FieldWidth = ?MAP_WIDTH / ?FIELDS_NUM,
    FieldHeight = ?MAP_HEIGHT / ?FIELDS_NUM,

    XOffset = round( (Position rem ?FIELDS_NUM) * FieldWidth ),
    YOffset = round( (Position div ?FIELDS_NUM) * FieldHeight ),

    X = XOffset + round( random:uniform(round(FieldWidth/2)) + FieldWidth/4 ),
    Y = YOffset + round( random:uniform(round(FieldHeight/2)) + FieldHeight/4 ),
    #stone{x=X, y=Y}.


milliseconds_diff(T2, T1) ->
    timer:now_diff(T2, T1) div 1000.

milliseconds_now() ->
    {Mg, S, Mc} = now(),
    (Mg * 1000000000) + (S * 1000) + (Mc div 1000).


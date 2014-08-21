-module (hunter_stone_manager).

-export ([create_stones/0]).

-define (MAP_WIDTH, 1350).
-define (MAP_HEIGHT, 1350).

-define (FIELDS_NUM, 5).

-include ("hunter_config.hrl").


create_stones() ->
    [get_random_stone(I) || I <- lists:seq(0, ?FIELDS_NUM * ?FIELDS_NUM - 1)].

get_random_stone(Position) ->
    FieldWidth = ?MAP_WIDTH / ?FIELDS_NUM,
    FieldHeight = ?MAP_HEIGHT / ?FIELDS_NUM,

    XOffset = round( (Position rem ?FIELDS_NUM) * FieldWidth ),
    YOffset = round( (Position div ?FIELDS_NUM) * FieldHeight ),

    X = XOffset + round( random:uniform(round(FieldWidth/2)) + FieldWidth/4 ),
    Y = YOffset + round( random:uniform(round(FieldHeight/2)) + FieldHeight/4 ),
    {X, Y}.

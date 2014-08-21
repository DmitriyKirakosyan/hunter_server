-module (hunter_stone_manager).

-export ([create_stones/0, update_stones/1,
          pick_stone/2, get_updated_stones_actions/2,
          get_actual_stones/1]).

-define (MAP_WIDTH, 1350).
-define (MAP_HEIGHT, 1350).

-define (FIELDS_NUM, 5).

-define (STONE_COOLDOWN, 10).

-include ("hunter_config.hrl").


create_stones() ->
    [get_random_stone(I) || I <- lists:seq(0, ?FIELDS_NUM * ?FIELDS_NUM - 1)].

update_stones(Stones) ->
    LastTime = ets:first(last_time),
    MillisecondsNow = hunter_utils:milliseconds_now(),
    ets:delete_all_objects(last_time),
    ets:insert(last_time, {MillisecondsNow}),

    TimeDelta = (MillisecondsNow - LastTime) / 1000,
    lists:map(
        fun(Stone) ->
            if
                Stone#stone.appearing_time < TimeDelta ->
                    Stone#stone{appearing_time=0};
                true -> Stone#stone{appearing_time = Stone#stone.appearing_time - TimeDelta}
            end
        end

    , Stones).

pick_stone({X, Y}, Stones) ->
    lists:map(
        fun(Stone) ->
            if
                X =:= Stone#stone.x andalso Y =:= Stone#stone.y ->
                    Stone#stone{appearing_time = ?STONE_COOLDOWN};
                true -> Stone
            end
        end
    , Stones).

get_actual_stones(Stones) ->
    lists:filter(
        fun(Stone) ->
            if
                Stone#stone.appearing_time =/= 0 ->
                    false;
                true -> true
            end
        end
    , Stones).

get_updated_stones_actions([], []) -> [];
get_updated_stones_actions([OldStone | OldStones], [NewStone | NewStones]) ->
    if
        OldStone#stone.appearing_time =:= 0 andalso NewStone#stone.appearing_time =/= 0 ->
            Action = [{action, ?STONE_REMOVED_ACTION}, {x, NewStone#stone.x}, {y, NewStone#stone.y}],
            [Action | get_updated_stones_actions(OldStones, NewStones)];
        OldStone#stone.appearing_time =/= 0 andalso NewStone#stone.appearing_time =:= 0 ->
            Action = [{action, ?STONE_ADDED_ACTION}, {x, NewStone#stone.x}, {y, NewStone#stone.y}],
            [Action | get_updated_stones_actions(OldStones, NewStones)];
        true ->
            get_updated_stones_actions(OldStones, NewStones)
    end.

get_random_stone(Position) ->
    FieldWidth = ?MAP_WIDTH / ?FIELDS_NUM,
    FieldHeight = ?MAP_HEIGHT / ?FIELDS_NUM,

    XOffset = round( (Position rem ?FIELDS_NUM) * FieldWidth ),
    YOffset = round( (Position div ?FIELDS_NUM) * FieldHeight ),

    X = XOffset + round( random:uniform(round(FieldWidth/2)) + FieldWidth/4 ),
    Y = YOffset + round( random:uniform(round(FieldHeight/2)) + FieldHeight/4 ),
    #stone{x=X, y=Y}.



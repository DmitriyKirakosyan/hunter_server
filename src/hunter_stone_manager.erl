-module (hunter_stone_manager).

-export ([create_stones/0, update_stones/2,
          pick_stone/2, get_updated_stones_actions/2,
          get_actual_stones/1]).

-define (MAP_WIDTH, 1350).
-define (MAP_HEIGHT, 1350).

-define (STONE_COOLDOWN, 10).

-include ("hunter_config.hrl").

create_stones() ->
    [get_random_stone(I) || I <- lists:seq(0, ?STONES_IN_ROW * ?STONES_IN_ROW - 1)].

update_stones(Stones, TimeDelta) ->
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
    FieldWidth = ?MAP_WIDTH / ?STONES_IN_ROW,
    FieldHeight = ?MAP_HEIGHT / ?STONES_IN_ROW,

    XOffset = round( (Position rem ?STONES_IN_ROW) * FieldWidth ),
    YOffset = round( (Position div ?STONES_IN_ROW) * FieldHeight ),

    X = XOffset + round( random:uniform(round(FieldWidth/2)) + FieldWidth/4 ),
    Y = YOffset + round( random:uniform(round(FieldHeight/2)) + FieldHeight/4 ),
    #stone{x=X, y=Y}.



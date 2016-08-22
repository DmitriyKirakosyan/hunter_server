-module (hunter_stone_manager).

-export ([create_stones/0, update_stones/2,
          pick_stone/2, get_updated_stones_actions/2,
          get_actual_stones/1]).

-define (STONE_COOLDOWN, 10).

-include ("hunter_config.hrl").

create_stones() ->
    [get_random_stone(I) || I <- lists:seq(0, ?STONES_IN_ROW * ?STONES_IN_ROW - 1)].

% update_stones(Stones, TimeDelta) ->
%     lists:map(
%         fun(#stone{appearing_time = StoneTime} = Stone) ->
%             if
%                 StoneTime < TimeDelta ->
%                     Stone#stone{appearing_time=0};
%                 true ->
%                     Stone#stone{appearing_time = StoneTime - TimeDelta}
%             end
%         end

%     , Stones).

update_stones([], _) -> [];
update_stones([#stone{appearing_time = StoneTime} = Stone | Stones], TimeDelta) when StoneTime < TimeDelta ->
    [Stone#stone{appearing_time=0} | update_stones(Stones, TimeDelta)];
update_stones([#stone{appearing_time = StoneTime} = Stone | Stones], TimeDelta) ->
    [Stone#stone{appearing_time = StoneTime - TimeDelta} | update_stones(Stones, TimeDelta)].

% pick_stone({X, Y}, Stones) ->
%     lists:map(
%         fun(Stone) ->
%             if
%                 X =:= Stone#stone.x andalso Y =:= Stone#stone.y ->
%                     Stone#stone{appearing_time = ?STONE_COOLDOWN};
%                 true -> Stone
%             end
%         end
%     , Stones).

pick_stone(_, []) -> [];
pick_stone({X, Y}, [#stone{x = StoneX, y = StoneY} = Stone | Stones]) when X =:= StoneX andalso Y =:= StoneY ->
    [Stone#stone{appearing_time = ?STONE_COOLDOWN} | pick_stone({X, Y}, Stones)];
pick_stone(XY, [Stone | Stones]) ->
    [Stone | pick_stone(XY, Stones)].

get_actual_stones(Stones) ->
    lists:filter(
        fun(#stone{appearing_time = StoneTime}) -> StoneTime =:= 0 end
    , Stones).


get_updated_stones_actions([], []) -> [];
get_updated_stones_actions([#stone{appearing_time = 0} | OldStones], [#stone{appearing_time = 0} | NewStones]) ->
    get_updated_stones_actions(OldStones, NewStones);
get_updated_stones_actions([#stone{appearing_time = 0} | OldStones], [#stone{x = X, y = Y} | NewStones]) ->
    Action = ?MAKE_STONE_REMOVED_ACTION(X, Y),
    [Action | get_updated_stones_actions(OldStones, NewStones)];
get_updated_stones_actions([_ | OldStones], [#stone{appearing_time = 0, x = X, y = Y} | NewStones]) ->
    Action = ?MAKE_STONE_ADDED_ACTION(X, Y),
    [Action | get_updated_stones_actions(OldStones, NewStones)];
get_updated_stones_actions([_ | OldStones], [_ | NewStones]) ->
    get_updated_stones_actions(OldStones, NewStones).


get_random_stone(Position) ->
    FieldWidth = ?MAP_WIDTH / ?STONES_IN_ROW,
    FieldHeight = ?MAP_HEIGHT / ?STONES_IN_ROW,

    XOffset = round( (Position rem ?STONES_IN_ROW) * FieldWidth ),
    YOffset = round( (Position div ?STONES_IN_ROW) * FieldHeight ),

    X = XOffset + round( rand:uniform(round(FieldWidth/2)) + FieldWidth/4 ),
    Y = YOffset + round( rand:uniform(round(FieldHeight/2)) + FieldHeight/4 ),
    #stone{x=X, y=Y}.



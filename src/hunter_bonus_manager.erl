-module (hunter_bonus_manager).

-export ([
            update/2,
            pick_bonus/2,
            get_updated_bonuses_actions/2
          ]).

-include ("hunter_config.hrl").

-type bonus() :: #bonus{}.

update(Bonuses, TimeDelta) when TimeDelta == 0 -> Bonuses;
update(Bonuses, TimeDelta) ->
    io:format("time delta : ~p~n", [TimeDelta]),
    RandomNum = rand:uniform(trunc(100 / TimeDelta) + 1),
        if
            RandomNum < 10 ->
                [get_random_bonus() | Bonuses];
            true -> Bonuses
        end.

pick_bonus({X, Y}, Bonuses) ->
    lists:filter(
        fun(Bonus) ->
            if
                X =:= Bonus#bonus.x andalso Y =:= Bonus#bonus.y ->
                    false;
                true -> true
            end
        end
    , Bonuses).

get_updated_bonuses_actions([], []) -> [];

get_updated_bonuses_actions([], [NewBonus | NewBonuses]) ->
    Action = [{action, ?BONUS_ADDED_ACTION}, {x, NewBonus#bonus.x}, {y, NewBonus#bonus.y}],
    [Action | get_updated_bonuses_actions([], NewBonuses)];

get_updated_bonuses_actions([OldBonus | OldBonuses], []) ->
    Action = [{action, ?BONUS_REMOVED_ACTION}, {x, OldBonus#bonus.x}, {y, OldBonus#bonus.y}],
    [Action | get_updated_bonuses_actions(OldBonuses, [])];

get_updated_bonuses_actions([OldBonus | OldBonuses], NewBonuses) ->
    case contains_bonus(OldBonus, NewBonuses) of
        false ->
            Action = [{action, ?BONUS_REMOVED_ACTION}, {x, OldBonus#bonus.x}, {y, OldBonus#bonus.y}],
            [Action | get_updated_bonuses_actions(OldBonuses, NewBonuses)];
        _ ->
            get_updated_bonuses_actions(OldBonuses, remove_bonus(OldBonus, NewBonuses))
    end.

-spec contains_bonus(bonus(), list()) -> true | false.
contains_bonus(BonusToFind, Bonuses) ->
    FoundBonuses = lists:filter(
        fun(Bonus) ->
            if
                BonusToFind#bonus.x =:= Bonus#bonus.x andalso BonusToFind#bonus.y =:= Bonus#bonus.y ->
                    true;
                true -> false
            end
        end
    , Bonuses),
    case FoundBonuses of
        [_FoundBonus] -> true;
        _ -> false
    end.

remove_bonus(Bonus, Bonuses) ->
    lists:filter(
        fun(Item) ->
            if
                Item#bonus.x =:= Bonus#bonus.x andalso Item#bonus.y =:= Bonus#bonus.y ->
                    false;
                true -> true
            end
        end
    , Bonuses).


get_random_bonus() ->
    X = rand:uniform ( round( ?MAP_WIDTH ) ),
    Y = rand:uniform( round( ?MAP_HEIGHT ) ),
    #bonus{x=X, y=Y, type=get_random_bonus_type()}.

get_random_bonus_type() ->
    Types = [?BONUS_HEAL, ?BONUS_SPEED],
    ListLength = lists:flatlength(Types),
    RandomIndex = rand:uniform(ListLength),
    lists:nth(RandomIndex, Types).


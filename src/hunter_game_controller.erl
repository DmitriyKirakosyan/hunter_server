-module (hunter_game_controller).

-behaviour (gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export ([action/1, logout/1]).

-include ("hunter_config.hrl").

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, hunter_game_controller}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, {[], hunter_stone_manager:create_stones(), {0, 0, 0}} }.%, 4000}.


%% StonesCounter -- debug only
handle_call({action, PlayerAction}, _From, {Players, Stones, {PickNum, AddedNum, RemovedNum}}) ->
    PlayerId = proplists:get_value(id, PlayerAction),
    ActionType = proplists:get_value(action, PlayerAction),
    io:format("action type : ~p~n", [ActionType]),
    
    TickedStones = hunter_stone_manager:update_stones(Stones),

    {UpdatedPlayers, UpdatedStones, UpdatedStonesCounter} = case ActionType of
        ?PING_ACTION ->
            {Players, TickedStones, {PickNum, AddedNum, RemovedNum}}; %% do nothing
        ?PICK_ACTION ->
            StoneX = get_number_from_action(x, PlayerAction),
            StoneY = get_number_from_action(y, PlayerAction),
            {Players, hunter_stone_manager:pick_stone({StoneX, StoneY}, TickedStones), {PickNum+1, AddedNum, RemovedNum}};

        _Else -> 
            {send_to_all(PlayerAction, Players), TickedStones, {PickNum, AddedNum, RemovedNum}}
    end,

    DiffStonesActions = hunter_stone_manager:get_updated_stones_actions(Stones, UpdatedStones),
    SysUpdatedPlayers = send_sys_actions_to_all(DiffStonesActions, UpdatedPlayers),

    FinalStonesCounter = update_stones_counter(DiffStonesActions, UpdatedStonesCounter),

    io:format("updated stones : ~p~n", [UpdatedStones]),

    {Player, NewPlayers} = get_or_create_player(PlayerId, SysUpdatedPlayers),
    Response = get_player_notifications(Player, ActionType, {Players, UpdatedStones}),
    FinalPlayers = replace_player(Player#player{notifications=[]}, NewPlayers),

    io:format("player action : ~p~n", [PlayerAction]),
    io:format("final players : ~p~n", [FinalPlayers]),
    io:format("stones counter : ~p~n", [FinalStonesCounter]),

    {reply, Response, {FinalPlayers, UpdatedStones, FinalStonesCounter}};

handle_call({logout, PlayerId}, _From, {Players, Stones, StonesCounter}) ->
    NewPlayers = remove_player(PlayerId, Players),
    LogoutAction = [{id, PlayerId}, {action, ?LOGOUT_ACTION}],
    FinalPlayers = send_sys_actions_to_all([LogoutAction], NewPlayers),
    {reply, ok, {FinalPlayers, Stones, StonesCounter}};


handle_call(Request, _From, State) ->
    io:format("wtf request? : ~p~n", [Request]),
    {reply, ok, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(Info, State) ->
    case Info of
        timeout ->
            {stop, [], State};
        _ ->
            {noreply, State}
    end.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Public functions
%%%===================================================================

action(PlayerAction) ->
    gen_server:call(hunter_game_controller, {action, PlayerAction}).

logout(PlayerId) ->
    gen_server:call(hunter_game_controller, {logout, PlayerId}).

%%%===================================================================
%%% Internal functions
%%%===================================================================


%% debug only
update_stones_counter([], StonesCounter) -> StonesCounter;
update_stones_counter([Action | Actions], {PickNum, AddedNum, RemovedNum}) ->
    ActionType = proplists:get_value(action, Action),
    case ActionType of
        ?STONE_ADDED_ACTION ->
            update_stones_counter(Actions, {PickNum, AddedNum+1, RemovedNum});
        ?STONE_REMOVED_ACTION ->
            update_stones_counter(Actions, {PickNum, AddedNum, RemovedNum+1});
        _Else ->
            update_stones_counter(Actions, {PickNum, AddedNum, RemovedNum})
    end.

remove_player(PlayerId, Players) ->
    lists:filter(
        fun(Player) ->
            Player#player.id =/= PlayerId
        end
    , Players).

send_to_all(Action, Players) ->
    PlayerId = proplists:get_value(id, Action),
    lists:map(
        fun(Player) ->
            if
                PlayerId =/= Player#player.id ->
                    Player#player{notifications=[Action | Player#player.notifications]};
                true -> Player
            end 
        end
    , Players).

send_sys_actions_to_all([], Players) -> Players;
send_sys_actions_to_all(Actions, Players) ->
    lists:map(
        fun(Player) ->
            Player#player{notifications=lists:concat([Player#player.notifications, Actions])}
        end
    , Players).


replace_player(Player, Players) ->
    NewPlayers = lists:filter(
        fun(Item) ->
            if Player#player.id =:= Item#player.id -> false; true -> true end
        end
    , Players),
    [Player | NewPlayers].

get_or_create_player(PlayerId, Players) ->
    case get_player(PlayerId, Players) of
        undefined ->
            NewPlayer = #player{id=PlayerId},
            {NewPlayer, [NewPlayer | Players]};
        Player ->
            {Player, Players}
    end.

get_player(_PlayerId, []) -> undefined;
get_player(PlayerId, [#player{id=PlayerId} = Player | _]) ->
    Player;
get_player(PlayerId, [_ | Players]) -> get_player(PlayerId, Players).


get_player_notifications(Player, ?LOGIN_ACTION, {_Players, Stones}) ->
    ActualStones = hunter_stone_manager:get_actual_stones(Stones),
    StoneActions = [[{action, ?STONE_ADDED_ACTION}, {x, Stone#stone.x}, {y, Stone#stone.y}]
                    || Stone <- ActualStones],
     io:format("stones data to send : ~p~n", [StoneActions]),
    UpdatedNotifications = hunter_actions_util:remove_actions([?STONE_ADDED_ACTION, ?STONE_REMOVED_ACTION], Player#player.notifications),
    get_player_notifications(Player#player{notifications = lists:concat([UpdatedNotifications, StoneActions]) });
get_player_notifications(Player, _, _) ->
    get_player_notifications(Player).

get_player_notifications(Player) ->
    hunter_actions_util:remove_actions_except_first(?MOVE_ACTION, Player#player.notifications).

get_number_from_action(Key, Action) ->
    Value = proplists:get_value(Key, Action),
    get_number_from_value(Value).

get_number_from_value(Value) when is_integer(Value) ->
    Value;
get_number_from_value(Value) when is_float(Value) ->
    round(Value);
get_number_from_value(Value) when is_binary(Value) ->
    ListValue = binary_to_list(Value),
    case string:to_integer(ListValue) of
        {error, no_integer} -> 0;
        {Int, _List} -> Int
    end;
get_number_from_value(Value) ->
    io:format("ERROR! unknown integer value : ~p~n", [Value]),
    0.

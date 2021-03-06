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
    {ok, #game_state{stones=hunter_stone_manager:create_stones()} }.%, 4000}.


handle_call({Action, Params, check_ignored_actions}, From, State) ->
    case lists:member(Action, State#game_state.debug#debug.ignore_actions) of
        true ->
            io:format("[Game Controller] Action ~p is in ignore list~n", [Action]),
            {reply, ?EMPTY_SERVER_RESPONSE, State};
        _false ->
            handle_call({Action, Params}, From, State)
    end;


%% Login
%% Send all actual stones back and session time left 
handle_call({?LOGIN_ACTION, PlayerAction} ,_From, State) ->
    TimeDelta = hunter_utils:get_time_delta(),

    PlayerId = proplists:get_value(id, PlayerAction),
    Name = proplists:get_value(name, PlayerAction),
    IsObs = proplists:get_value(obs, PlayerAction, false),

    PlayersWithNewcomer = add_new_player(PlayerId, Name, IsObs, State#game_state.players),

    GameInfoAction = ?MAKE_GAME_INFO_ACTION(convert_players_for_response(State#game_state.players), ?MAP_WIDTH, ?MAP_HEIGHT),
    NewPlayers = send_sys_action(GameInfoAction, PlayerId, PlayersWithNewcomer),

    RunGameState = run_game(State#game_state{players=NewPlayers}),

    TimedPlayers = send_sys_action_to_all(?MAKE_TIME_ACTION(RunGameState#game_state.time_left), RunGameState#game_state.players),

    SentState = send_action_to_all(PlayerAction, RunGameState#game_state{players = TimedPlayers}),
    {Response, UpdatedState} = play_action(PlayerAction, TimeDelta, SentState),

    {reply, Response, UpdatedState};

handle_call({?DEAD_ACTION, PlayerAction} ,_From, State) ->
    TimeDelta = hunter_utils:get_time_delta(),
    
    SentState = send_action_to_all(PlayerAction, State),
    {Response, UpdatedState} = play_action(PlayerAction, TimeDelta, SentState),

    {reply, Response, UpdatedState#game_state{results=[PlayerAction | UpdatedState#game_state.results]}};

handle_call({?PING_ACTION, PlayerAction} ,_From, State) ->
    TimeDelta = hunter_utils:get_time_delta(),

    {Response, UpdatedState} = play_action(PlayerAction, TimeDelta, State),

    {reply, Response, UpdatedState};

%% action of picking weapon by player
handle_call({?PICK_WEAPON_ACTION, PlayerAction} ,_From, State) ->
    TimeDelta = hunter_utils:get_time_delta(),

    StoneX = get_number_from_action(x, PlayerAction),
    StoneY = get_number_from_action(y, PlayerAction),
    NewStones = hunter_stone_manager:pick_stone({StoneX, StoneY}, State#game_state.stones),

    SentState = send_action_to_all(PlayerAction, State#game_state{stones=NewStones}),
    {Response, UpdatedState} = play_action(PlayerAction, TimeDelta, SentState),

    {reply, Response, UpdatedState};

%% action of picking bonus by player
handle_call({?PICK_BONUS_ACTION, PlayerAction} ,_From, State) ->
    TimeDelta = hunter_utils:get_time_delta(),

    BonusX = get_number_from_action(x, PlayerAction),
    BonusY = get_number_from_action(y, PlayerAction),
    NewBonuses = hunter_bonus_manager:pick_bonus({BonusX, BonusY}, State#game_state.bonuses),

    SentState = send_action_to_all(PlayerAction, State#game_state{bonuses=NewBonuses}),
    {Response, UpdatedState} = play_action(PlayerAction, TimeDelta, SentState),

    {reply, Response, UpdatedState};

handle_call({logout, PlayerId}, _From, #game_state{players=Players} = State) ->
    NewPlayers = remove_player(PlayerId, Players),
    LogoutAction = [{id, PlayerId}, {action, ?LOGOUT_ACTION}],
    FinalPlayers = send_sys_actions_to_all([LogoutAction], NewPlayers),
    {reply, ok, State#game_state{players=FinalPlayers}};

handle_call({ActionType, PlayerAction} ,_From, State) when is_binary(ActionType) ->
    TimeDelta = hunter_utils:get_time_delta(),
    
    SentState = send_action_to_all(PlayerAction, State),
    {Response, UpdatedState} = play_action(PlayerAction, TimeDelta, SentState),

    {reply, Response, UpdatedState};


%%% admin

handle_call({admin_ignore_action, Action}, From, State) when is_atom(Action) ->
    BinaryAction = atom_to_binary(Action, utf8),
    handle_call({admin_ignore_action, BinaryAction}, From, State);

handle_call({admin_ignore_action, Action}, _From, State) ->
    IgnoredActions = [Action| lists:delete(Action, State#game_state.debug#debug.ignore_actions)],
    {reply, ok, State#game_state{debug = State#game_state.debug#debug{ignore_actions = IgnoredActions}}};

handle_call(admin_clear_ignored_actions, _From, State) ->
    {reply, ok, State#game_state{debug = State#game_state.debug#debug{ignore_actions = []}}};

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
    ActionType = proplists:get_value(action, PlayerAction),
    gen_server:call(hunter_game_controller, {ActionType, PlayerAction, check_ignored_actions}).

logout(PlayerId) ->
    gen_server:call(hunter_game_controller, {logout, PlayerId}).


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% return: {Response, UpdatedState}
play_action(PlayerAction, TimeDelta, State) ->
    PlayerId = proplists:get_value(id, PlayerAction),
    TickedState = tick_state(TimeDelta, State),
    case get_player(PlayerId, TickedState#game_state.players) of
        undefined ->
            io:format("ERROR! Player not found. action : ~p~n", [PlayerAction]),
            {[], State};
        Player ->
            io:format("ticked state : ~p~n", [TickedState]),
            ActionType = proplists:get_value(action, PlayerAction),
            Response = create_response(ActionType, Player, TickedState),
            UpdatedPlayers = replace_player(Player#player{notifications=[]}, TickedState#game_state.players),
            UpdatedState = TickedState#game_state{players=UpdatedPlayers},
            FinalState = update_game_state(TimeDelta, UpdatedState),
            io:format("response : ~p~n", [Response]),
            io:format("final state : ~p~n", [FinalState]),
            {Response, FinalState}
    end.

%% Send action to all players except sender
%%
send_action_to_all(PlayerAction, State) ->
    State#game_state{players=send_to_all(PlayerAction, State#game_state.players)}.

%% state sections :

%% Starts game if it's more then 1 playr and not started yet
%%

run_game(#game_state{players=[_, _ | _], started=false} = State) ->
    State#game_state{time_left=?PLAY_TIME, started=true};
run_game(State) ->
    State.


tick_state(TimeDelta, State) ->
    StoneTickedState = tick_stones(TimeDelta, State),
    tick_bonuses(TimeDelta, StoneTickedState).

tick_stones(TimeDelta, State) ->
    TickedStones = hunter_stone_manager:update_stones(State#game_state.stones, TimeDelta),

    DiffStonesActions = hunter_stone_manager:get_updated_stones_actions(State#game_state.stones, TickedStones),
    NewPlayers = send_sys_actions_to_all(DiffStonesActions, State#game_state.players),

    %io:format("Updated stones : ~p~n", [TickedStones]),

    %% NOTE: debug
    NewDebug = hunter_debug_util:count_stones(DiffStonesActions, State#game_state.debug),

    State#game_state{players=NewPlayers, stones=TickedStones, debug=NewDebug}.

tick_bonuses(TimeDelta, State) ->
    TickedBonuses = hunter_bonus_manager:update(State#game_state.bonuses, TimeDelta),

    DiffBonusesActions = hunter_bonus_manager:get_updated_bonuses_actions(State#game_state.bonuses, TickedBonuses),
    NewPlayers = send_sys_actions_to_all(DiffBonusesActions, State#game_state.players),

    State#game_state{players=NewPlayers, bonuses=TickedBonuses}.

%% Returns response for client and updated State
%% return: {Response, UpdatedState}
create_response(ActionType, Player, State) ->
    NewPlayers = replace_player(Player#player{notifications=[]}, State#game_state.players),
    hunter_notifications:calc_notifications(Player, ActionType, {NewPlayers, State#game_state.stones}).

%% Checks if it is end of the game and returns updated State
%%
update_game_state(TimeDelta, State) ->
    UpdatedTime = update_left_time(TimeDelta, State),
    io:format("time left : ~p~n", [UpdatedTime]),
    if
        UpdatedTime =< 0 andalso State#game_state.started =:= true ->
            ResultAction = hunter_score_manager:get_result_action(State#game_state.results, State#game_state.players),
            io:format("result action : ~p~n", [ResultAction]),
            ResultedPlayers = send_sys_actions_to_all([ResultAction], State#game_state.players),
            State#game_state{players=ResultedPlayers, started=false, time_left=0, results=[]};
        true ->
            State#game_state{time_left=UpdatedTime}
    end.


update_left_time(_TimeDelta, #game_state{started=false}) -> 0;
update_left_time(TimeDelta, #game_state{time_left=TimeLeft}) ->
    if
        TimeDelta < TimeLeft ->
            TimeLeft - TimeDelta;
        true -> 0
    end.

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

send_sys_action(Action, PlayerId, Players) ->
    lists:map(
        fun(Player) ->
            if
                Player#player.id =:= PlayerId ->
                    Player#player{notifications = [Action | Player#player.notifications]};
                true -> Player
            end
        end
    , Players).

send_sys_action_to_all(Action, Players) -> send_sys_actions_to_all([Action], Players).

send_sys_actions_to_all([], Players) -> Players;
send_sys_actions_to_all(Actions, Players) ->
    lists:map(
        fun(Player) ->
            Player#player{notifications=lists:concat([Player#player.notifications, Actions])}
        end
    , Players).

%% Response utils

convert_players_for_response(Players) ->
    lists:map(
        fun(#player{id = Id, name = Name, is_obs = Obs, is_bot = Bot}) ->
            {struct, [{id, Id}, {name, Name}, {obs, Obs}, {bot, Bot}]}
        end

    , Players).

%% Player creation and adding

remove_player(PlayerId, Players) ->
    lists:filter(
        fun(Player) ->
            Player#player.id =/= PlayerId
        end
    , Players).

replace_player(Player, Players) ->
    NewPlayers = lists:filter(
        fun(Item) ->
            if Player#player.id =:= Item#player.id -> false; true -> true end
        end
    , Players),
    [Player | NewPlayers].

%% creating new player and replace if exists
-spec add_new_player(binary(), binary(), boolean(), list(#player{})) -> list(#player{}).
add_new_player(PlayerId, Name, IsObs, Players) ->
    NewPlayer = #player{id=PlayerId, name=Name, is_obs = IsObs},
    replace_player(NewPlayer, Players).

get_player(_PlayerId, []) -> undefined;
get_player(PlayerId, [#player{id=PlayerId} = Player | _]) ->
    Player;
get_player(PlayerId, [_ | Players]) -> get_player(PlayerId, Players).


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

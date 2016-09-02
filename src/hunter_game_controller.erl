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

-record (state, {
    players = [],
    stones = [],
    bonuses = [],
    results = [],
    time_left = 0,
    started = false,
    debug = #debug{}
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, hunter_game_controller}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{stones=hunter_stone_manager:create_stones()} }.%, 4000}.


%% State :
%%   players
%%   stones
%%   debug
%%   results
%%   time_left
%%   started


handle_call({Action, Params, check_ignored_actions}, From, State) ->
    case lists:member(Action, State#state.debug#debug.ignore_actions) of
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

    PlayersWithNewcomer = add_new_player(PlayerId, Name, State#state.players),

    GameInfoAction = ?MAKE_GAME_INFO_ACTION(convert_players_for_response(State#state.players)),
    NewPlayers = send_sys_action(GameInfoAction, PlayerId, PlayersWithNewcomer),

    RunGameState = run_game(State#state{players=NewPlayers}),

    TimedPlayers = send_sys_action_to_all(?MAKE_TIME_ACTION(RunGameState#state.time_left), RunGameState#state.players),

    SentState = send_action_to_all(PlayerAction, RunGameState#state{players = TimedPlayers}),
    {Response, UpdatedState} = play_action(PlayerAction, TimeDelta, SentState),

    {reply, Response, UpdatedState};

handle_call({?DEAD_ACTION, PlayerAction} ,_From, State) ->
    TimeDelta = hunter_utils:get_time_delta(),
    
    SentState = send_action_to_all(PlayerAction, State),
    {Response, UpdatedState} = play_action(PlayerAction, TimeDelta, SentState),

    {reply, Response, UpdatedState#state{results=[PlayerAction | UpdatedState#state.results]}};

handle_call({?PING_ACTION, PlayerAction} ,_From, State) ->
    TimeDelta = hunter_utils:get_time_delta(),

    {Response, UpdatedState} = play_action(PlayerAction, TimeDelta, State),

    {reply, Response, UpdatedState};

%% action of picking weapon by player
handle_call({?PICK_WEAPON_ACTION, PlayerAction} ,_From, State) ->
    TimeDelta = hunter_utils:get_time_delta(),

    StoneX = get_number_from_action(x, PlayerAction),
    StoneY = get_number_from_action(y, PlayerAction),
    NewStones = hunter_stone_manager:pick_stone({StoneX, StoneY}, State#state.stones),

    SentState = send_action_to_all(PlayerAction, State#state{stones=NewStones}),
    {Response, UpdatedState} = play_action(PlayerAction, TimeDelta, SentState),

    {reply, Response, UpdatedState};

%% action of picking bonus by player
handle_call({?PICK_BONUS_ACTION, PlayerAction} ,_From, State) ->
    TimeDelta = hunter_utils:get_time_delta(),

    BonusX = get_number_from_action(x, PlayerAction),
    BonusY = get_number_from_action(y, PlayerAction),
    NewBonuses = hunter_bonus_manager:pick_bonus({BonusX, BonusY}, State#state.bonuses),

    SentState = send_action_to_all(PlayerAction, State#state{bonuses=NewBonuses}),
    {Response, UpdatedState} = play_action(PlayerAction, TimeDelta, SentState),

    {reply, Response, UpdatedState};

handle_call({logout, PlayerId}, _From, #state{players=Players} = State) ->
    NewPlayers = remove_player(PlayerId, Players),
    LogoutAction = [{id, PlayerId}, {action, ?LOGOUT_ACTION}],
    FinalPlayers = send_sys_actions_to_all([LogoutAction], NewPlayers),
    {reply, ok, State#state{players=FinalPlayers}};

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
    IgnoredActions = [Action| lists:delete(Action, State#state.debug#debug.ignore_actions)],
    {reply, ok, State#state{debug = State#state.debug#debug{ignore_actions = IgnoredActions}}};

handle_call(admin_clear_ignored_actions, _From, State) ->
    {reply, ok, State#state{debug = State#state.debug#debug{ignore_actions = []}}};

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
    case get_player(PlayerId, TickedState#state.players) of
        undefined ->
            io:format("ERROR! Player not found. action : ~p~n", [PlayerAction]),
            {[], State};
        Player ->
            io:format("ticked state : ~p~n", [TickedState]),
            ActionType = proplists:get_value(action, PlayerAction),
            Response = create_response(ActionType, Player, TickedState),
            UpdatedPlayers = replace_player(Player#player{notifications=[]}, TickedState#state.players),
            UpdatedState = TickedState#state{players=UpdatedPlayers},
            FinalState = update_game_state(TimeDelta, UpdatedState),
            io:format("response : ~p~n", [Response]),
            io:format("final state : ~p~n", [FinalState]),
            {Response, FinalState}
    end.

%% Send action to all players except sender
%%
send_action_to_all(PlayerAction, State) ->
    State#state{players=send_to_all(PlayerAction, State#state.players)}.

%% state sections :

%% Starts game if it's more then 1 playr and not started yet
%%

run_game(#state{players=[_, _ | _], started=false} = State) ->
    State#state{time_left=?PLAY_TIME, started=true};
run_game(State) ->
    State.


tick_state(TimeDelta, State) ->
    StoneTickedState = tick_stones(TimeDelta, State),
    tick_bonuses(TimeDelta, StoneTickedState).

tick_stones(TimeDelta, State) ->
    TickedStones = hunter_stone_manager:update_stones(State#state.stones, TimeDelta),

    DiffStonesActions = hunter_stone_manager:get_updated_stones_actions(State#state.stones, TickedStones),
    NewPlayers = send_sys_actions_to_all(DiffStonesActions, State#state.players),

    %io:format("Updated stones : ~p~n", [TickedStones]),

    %% NOTE: debug
    NewDebug = hunter_debug_util:count_stones(DiffStonesActions, State#state.debug),

    State#state{players=NewPlayers, stones=TickedStones, debug=NewDebug}.

tick_bonuses(TimeDelta, State) ->
    TickedBonuses = hunter_bonus_manager:update(State#state.bonuses, TimeDelta),

    DiffBonusesActions = hunter_bonus_manager:get_updated_bonuses_actions(State#state.bonuses, TickedBonuses),
    NewPlayers = send_sys_actions_to_all(DiffBonusesActions, State#state.players),

    State#state{players=NewPlayers, bonuses=TickedBonuses}.

%% Returns response for client and updated State
%% return: {Response, UpdatedState}
create_response(ActionType, Player, State) ->
    NewPlayers = replace_player(Player#player{notifications=[]}, State#state.players),
    hunter_notifications:calc_notifications(Player, ActionType, {NewPlayers, State#state.stones}).

%% Checks if it is end of the game and returns updated State
%%
update_game_state(TimeDelta, State) ->
    UpdatedTime = update_left_time(TimeDelta, State),
    io:format("time left : ~p~n", [UpdatedTime]),
    if
        UpdatedTime =< 0 andalso State#state.started =:= true ->
            ResultAction = hunter_score_manager:get_result_action(State#state.results, State#state.players),
            io:format("result action : ~p~n", [ResultAction]),
            ResultedPlayers = send_sys_actions_to_all([ResultAction], State#state.players),
            State#state{players=ResultedPlayers, started=false, time_left=0, results=[]};
        true ->
            State#state{time_left=UpdatedTime}
    end.


update_left_time(_TimeDelta, #state{started=false}) -> 0;
update_left_time(TimeDelta, #state{time_left=TimeLeft}) ->
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
        fun(Player) ->
            {struct, [{id, Player#player.id}, {name, Player#player.name}]}
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
add_new_player(PlayerId, Name, Players) ->
    NewPlayer = #player{id=PlayerId, name=Name},
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

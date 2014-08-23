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
    results = [],
    time_left = 0,
    started = false,
    debug = #debug{}
}).

-define (PLAY_TIME, 10).

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

handle_call({?LOGIN_ACTION, PlayerAction} ,_From, State) ->
    TimeDelta = hunter_utils:get_time_delta(),

    PlayerId = proplists:get_value(id, PlayerAction),
    Name = proplists:get_value(name, PlayerAction),
    NewPlayers = add_new_player(PlayerId, Name, State#state.players),

    StartedState = run_game(State#state{players=NewPlayers}),

    SendedState = send_action_to_all(PlayerAction, StartedState),
    {Response, UpdatedState} = play_action(PlayerAction, TimeDelta, SendedState),

    {reply, Response, UpdatedState};

handle_call({?DEAD_ACTION, PlayerAction} ,_From, #state{started=false} = State) ->
    TimeDelta = hunter_utils:get_time_delta(),
    
    SendedState = send_action_to_all(PlayerAction, State),
    {Response, UpdatedState} = play_action(PlayerAction, TimeDelta, SendedState),

    {reply, Response, UpdatedState#state{results=[PlayerAction | UpdatedState#state.results]}};

handle_call({?PING_ACTION, PlayerAction} ,_From, #state{started=false} = State) ->
    TimeDelta = hunter_utils:get_time_delta(),

    {Response, UpdatedState} = play_action(PlayerAction, TimeDelta, State),

    {reply, Response, UpdatedState};

handle_call({?PICK_ACTION, PlayerAction} ,_From, #state{started=false} = State) ->
    TimeDelta = hunter_utils:get_time_delta(),

    StoneX = get_number_from_action(x, PlayerAction),
    StoneY = get_number_from_action(y, PlayerAction),
    NewStones = hunter_stone_manager:pick_stone({StoneX, StoneY}, State#state.stones),

    SendedState = send_action_to_all(PlayerAction, State#state{stones=NewStones}),
    {Response, UpdatedState} = play_action(PlayerAction, TimeDelta, SendedState),

    {reply, Response, UpdatedState};

handle_call({_ActionType, PlayerAction} ,_From, #state{started=false} = State) ->
    TimeDelta = hunter_utils:get_time_delta(),
    
    SendedState = send_action_to_all(PlayerAction, State),
    {Response, UpdatedState} = play_action(PlayerAction, TimeDelta, SendedState),

    {reply, Response, UpdatedState};

handle_call({logout, PlayerId}, _From, #state{players=Players} = State) ->
    NewPlayers = remove_player(PlayerId, Players),
    LogoutAction = [{id, PlayerId}, {action, ?LOGOUT_ACTION}],
    FinalPlayers = send_sys_actions_to_all([LogoutAction], NewPlayers),
    {reply, ok, State#state{players=FinalPlayers}};


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
    gen_server:call(hunter_game_controller, {ActionType, PlayerAction}).

logout(PlayerId) ->
    gen_server:call(hunter_game_controller, {logout, PlayerId}).

%%%===================================================================
%%% Internal functions
%%%===================================================================


%% return: {Response, UpdatedState}
play_action(PlayerAction, TimeDelta, State) ->
    PlayerId = proplists:get_value(id, PlayerAction),
    case get_player(PlayerId, State#state.players) of
        undefined ->
            io:format("ERROR! Player not found. action : ~p~n", [PlayerAction]),
            State;
        Player ->
            ActionType = proplists:get_value(action, PlayerAction),
            TickedStonesState = tick_stones(TimeDelta, State),
            {Response, UpdatedState2} = create_response(ActionType, Player, TickedStonesState),
            {Response, update_game_state(TimeDelta, UpdatedState2)}
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


tick_stones(TimeDelta, State) ->
    TickedStones = hunter_stone_manager:update_stones(State#state.stones, TimeDelta),

    DiffStonesActions = hunter_stone_manager:get_updated_stones_actions(State#state.stones, TickedStones),
    NewPlayers = send_sys_actions_to_all(DiffStonesActions, State#state.players),

    %% NOTE: debug
    NewDebug = hunter_debug_util:count_stones(DiffStonesActions, State#state.debug),

    State#state{players=NewPlayers, stones=TickedStones, debug=NewDebug}.

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

%% creating new player and replace if exists
add_new_player(PlayerId, Name, Players) ->
    NewPlayer = #player{id=PlayerId, name=Name},
    {NewPlayer, replace_player(NewPlayer, Players)}.

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

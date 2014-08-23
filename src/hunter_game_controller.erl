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
handle_call({action, PlayerAction}, _From, State) ->
    PlayerId = proplists:get_value(id, PlayerAction),
    Name = proplists:get_value(name, PlayerAction), %% TODO: the name comes only in login request
    ActionType = proplists:get_value(action, PlayerAction),

    Players = State#state.players,
    Stones = State#state.stones,
    Debug = State#state.debug,

    TimeDelta = hunter_utils:get_time_delta(),

    io:format("action type : ~p~n", [ActionType]),

    {_Player, NewPlayers} = get_or_create_player(PlayerId, Name, Players),
    
    TickedStones = hunter_stone_manager:update_stones(Stones, TimeDelta),

    PlayersNum = lists:flatlength(NewPlayers),

    TicketStonesState = State#state{stones=TickedStones},
    UpdatedState = case ActionType of
	?DEAD_ACTION ->
	    TicketStonesState#state{players=send_to_all(PlayerAction, NewPlayers),
				    results=[PlayerAction | TicketStonesState#state.results]};
        ?LOGIN_ACTION when PlayersNum > 1 andalso State#state.started =:= false ->
            %% starting a new game
            TicketStonesState#state{players=send_to_all(PlayerAction, NewPlayers), time_left=?PLAY_TIME, started=true};
        ?PING_ACTION ->
            TicketStonesState; %% do nothing
        ?PICK_ACTION ->
            StoneX = get_number_from_action(x, PlayerAction),
            StoneY = get_number_from_action(y, PlayerAction),
            TicketStonesState#state{stones = hunter_stone_manager:pick_stone({StoneX, StoneY}, TickedStones)};
        _Else ->
            TicketStonesState#state{players=send_to_all(PlayerAction, NewPlayers)}
    end,

    DiffStonesActions = hunter_stone_manager:get_updated_stones_actions(Stones, UpdatedState#state.stones),
    SysUpdatedPlayers = send_sys_actions_to_all(DiffStonesActions, UpdatedState#state.players),

    io:format("updated stones : ~p~n", [UpdatedState#state.stones]),

    {NewPlayer, _} = get_or_create_player(PlayerId, Name, SysUpdatedPlayers),
    Response = hunter_notifications:calc_notifications(NewPlayer, ActionType, {SysUpdatedPlayers, UpdatedState#state.stones}),
    FinalPlayers = replace_player(NewPlayer#player{notifications=[]}, SysUpdatedPlayers),

    %% NOTE: debug
    NewDebug = hunter_debug_util:update(ActionType, DiffStonesActions, TimeDelta, Debug),

    io:format("player action : ~p~n", [PlayerAction]),
    io:format("final players : ~p~n", [FinalPlayers]),
    io:format("debug info : ~p~n", [NewDebug]),

    io:format("state : ~p~n", [UpdatedState]),
    UpdatedTime = update_left_time(TimeDelta, UpdatedState),

    io:format("time left : ~p~n", [UpdatedTime]),

    PreFinalState = UpdatedState#state{players=FinalPlayers, stones=UpdatedState#state.stones, time_left=UpdatedTime, debug=NewDebug},
    FinalState = if
        UpdatedTime =< 0 andalso PreFinalState#state.started =:= true ->
            ResultAction = hunter_score_manager:get_result_action(PreFinalState#state.results, PreFinalState#state.players),
            io:format("result action : ~p~n", [ResultAction]),
            ResultedPlayers = send_sys_actions_to_all([ResultAction], PreFinalState#state.players),
            PreFinalState#state{players=ResultedPlayers, started=false, time_left=0, results=[]};
        true ->
            PreFinalState#state{time_left=UpdatedTime}
    end,

    {reply, Response, FinalState};

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
    gen_server:call(hunter_game_controller, {action, PlayerAction}).

logout(PlayerId) ->
    gen_server:call(hunter_game_controller, {logout, PlayerId}).

%%%===================================================================
%%% Internal functions
%%%===================================================================

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

get_or_create_player(PlayerId, Name, Players) ->
    case get_player(PlayerId, Players) of
        undefined ->
            NewPlayer = #player{id=PlayerId, name=Name},
            {NewPlayer, [NewPlayer | Players]};
        Player ->
            {Player, Players}
    end.

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

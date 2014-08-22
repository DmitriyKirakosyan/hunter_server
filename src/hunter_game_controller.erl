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
    debug = #stones_counter{}
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


%% StonesCounter -- debug only
handle_call({action, PlayerAction}, _From, #state{players=Players, stones=Stones, debug=Debug}) ->
    PlayerId = proplists:get_value(id, PlayerAction),
    ActionType = proplists:get_value(action, PlayerAction),
    io:format("action type : ~p~n", [ActionType]),
    
    TickedStones = hunter_stone_manager:update_stones(Stones),

    {UpdatedPlayers, UpdatedStones} = case ActionType of
        ?PING_ACTION ->
            {Players, TickedStones}; %% do nothing
        ?PICK_ACTION ->
            StoneX = get_number_from_action(x, PlayerAction),
            StoneY = get_number_from_action(y, PlayerAction),
            {Players, hunter_stone_manager:pick_stone({StoneX, StoneY}, TickedStones)};

        _Else -> 
            {send_to_all(PlayerAction, Players), TickedStones}
    end,

    DiffStonesActions = hunter_stone_manager:get_updated_stones_actions(Stones, UpdatedStones),
    SysUpdatedPlayers = send_sys_actions_to_all(DiffStonesActions, UpdatedPlayers),


    io:format("updated stones : ~p~n", [UpdatedStones]),

    {Player, NewPlayers} = get_or_create_player(PlayerId, SysUpdatedPlayers),
    Response = hunter_notifications:calc_notifications(Player, ActionType, {Players, UpdatedStones}),
    FinalPlayers = replace_player(Player#player{notifications=[]}, NewPlayers),

    NewDebug = debug_util:count_stones(ActionType, DiffStonesActions, Debug),

    io:format("player action : ~p~n", [PlayerAction]),
    io:format("final players : ~p~n", [FinalPlayers]),
    io:format("stones counter : ~p~n", [NewDebug]),

    {reply, Response, #state{players=FinalPlayers, stones=UpdatedStones, debug=NewDebug}};

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

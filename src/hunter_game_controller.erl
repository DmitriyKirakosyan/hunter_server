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

-export ([action/1]).

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
    {ok, {[], hunter_stone_manager:create_stones()}}.%, 4000}.



handle_call({action, PlayerAction}, _From, {Players, Stones}) ->
    PlayerId = proplists:get_value(<<"id">>, PlayerAction),
    ActionType = proplists:get_value(<<"action">>, PlayerAction),
    io:format("action type : ~p~n", [ActionType]),
    
    {UpdatedPlayers, UpdatedStones} = case ActionType of
        ?PING_ACTION ->
            {Players, Stones}; %% do nothing
        ?PICK_ACTION ->
            StoneX = get_number_from_action(<<"x">>, PlayerAction),
            StoneY = get_number_from_action(<<"y">>, PlayerAction),
            {Players, hunter_stone_manager:pick_stone({StoneX, StoneY}, Stones)};
        _Else -> 
            {send_to_all(PlayerAction, Players), Stones}
    end,

    TickedStones = hunter_stone_manager:update_stones(UpdatedStones),

    {Player, NewPlayers} = get_or_create_player(PlayerId, UpdatedPlayers),
    Response = get_player_notifications(Player, ActionType, {Players, TickedStones}),
    FinalPlayers = replace_player(Player#player{notifications=[]}, NewPlayers),

    io:format("player action : ~p~n", [PlayerAction]),
    io:format("final players : ~p~n", [FinalPlayers]),

    {reply, Response, {FinalPlayers, Stones}};

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

%%%===================================================================
%%% Internal functions
%%%===================================================================

send_to_all(Params, Players) ->
    PlayerId = proplists:get_value(<<"id">>, Params),
    lists:map(
        fun(Player) ->
            if
                PlayerId =/= Player#player.id ->
                    Player#player{notifications=[Params | Player#player.notifications]};
                true -> Player
            end 
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

get_player_notifications(Player, ActionType, {_Players, Stones}) ->
    StonesData = if
        ActionType =:= ?LOGIN_ACTION ->
            %% will be added "struct" before mochi converting
            [[{action, ?STONE_ADDED_ACTION}, {x, Stone#stone.x}, {y, Stone#stone.y}] || Stone <- Stones];
        true -> []
    end,
    lists:concat([Player#player.notifications, StonesData]).

get_number_from_action(Key, Action) ->
    BinValue = proplists:get_value(Key, Action),
    ListValue = binary_to_list(BinValue),
    case string:to_integer(ListValue) of
        {error, no_integer} -> 0;
        {Int, _List} -> Int
    end.

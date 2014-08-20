-module(hunter_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

-include("hunter_config.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    listen(8080).

stop(_State) ->
    ok.

%% ===================================================================
%% Internal logic
%% ===================================================================


listen(Port) ->
    {ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    io:format("socket accepted!~n"),
    accept(LSocket).

% Wait for incoming connections and spawn the echo loop when we get one.
accept(LSocket) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    spawn(fun() -> loop(Socket, {[], []}) end),
    accept(LSocket).

% Echo back whatever data we receive on Socket.
loop(Socket, {Players, Stones}) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            {struct, Params} = mochijson2:decode(Data),
            PlayerId = proplists:get(<<"id">>, Params),
            SendedPlayers = send_to_all(Params, Players),
            {Player, NewPlayers} = get_or_create_player(PlayerId, SendedPlayers),
            Respond = Player#player.notifications,
            FinalPlayers = replace_player(Player#player{notifications=[]}, NewPlayers),


            io:format("data recieved : ~p~n", [Data]),
            io:format("decoded data : ~p~n", [Params]),
            io:format("final players : ~p~n", [FinalPlayers]),

            gen_tcp:send(Socket, Respond),
            loop(Socket, {FinalPlayers, Stones});
        {error, closed} ->
            ok
    end.

send_to_all(Params, Players) ->
    PlayerId = proplists:get(<<"id">>, Params),
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
-module(hunter_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-export([divide_actions/1]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

-include("hunter_config.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ets:new(last_time, [public, named_table]),
    ets:insert(last_time, {hunter_utils:milliseconds_now()}),
    hunter_sup:start_link(),
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
    spawn(fun() -> loop(Socket, undefined) end),
    accept(LSocket).

% Echo back whatever data we receive on Socket.
loop(Socket, PlayerId) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            io:format("data recieved : ~p~n", [Data]),
            MochiActions = [mochijson2:decode(Item) || Item <- divide_actions(Data)],
            Actions = [translate_keys_to_atom(Item) || {struct, Item} <- MochiActions],

            %{struct, Params} = mochijson2:decode(Data),
            io:format("decoded request : ~p~n", [Actions]),

            ResponseList = [hunter_game_controller:action(Item) || Item <- Actions],
            Response = lists:concat(ResponseList),
            MochiResponse = [{struct, Item} || Item <- Response],
            io:format("response : ~p~n", [Response]),
            io:format("mochi response : ~p~n", [MochiResponse]),
            io:format("encoded response : ~p~n", [mochijson2:encode(lists:reverse(MochiResponse))]),

            gen_tcp:send(Socket, mochijson2:encode(lists:reverse(MochiResponse))),
            if
                PlayerId =:= undefined ->
                    PlayerAction = lists:last(Actions),
                    loop(Socket, proplists:get_value(id, PlayerAction));
                true ->
                    loop(Socket, PlayerId)
            end;
        {error, closed} when PlayerId =/= undefined ->
            io:format("connection closed from : ~p~n", [PlayerId]),
            hunter_game_controller:logout(PlayerId);
        {error, closed} ->
            io:format("connection closed~n")
    end.

translate_keys_to_atom(Action) ->
    lists:map(
        fun(Item) ->
            case Item of
                {Key, Value} when is_binary(Key) ->
                    {binary_to_atom(Key, utf8), Value};
                Else -> Else
            end
        end
    , Action).

divide_actions(Actions) ->
    case binary:split(Actions, <<"}{">>) of
        [Action, Tail] -> [<<Action/binary,"}">> | divide_actions(<<"{",Tail/binary>>)];
        Else -> Else
    end.


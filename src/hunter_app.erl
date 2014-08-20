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
    spawn(fun() -> loop(Socket) end),
    accept(LSocket).

% Echo back whatever data we receive on Socket.
loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            io:format("data recieved : ~p~n", [Data]),
            MochiActions = [mochijson2:decode(Item) || Item <- divide_actions(Data)],
            Actions = [Item || {struct, Item} <- MochiActions],

            %{struct, Params} = mochijson2:decode(Data),
            io:format("mochi request : ~p~n", [MochiActions]),

            ResponseList = [hunter_game_controller:action(Item) || Item <- Actions],
            Response = lists:concat(ResponseList),
            MochiResponse = [{struct, Item} || Item <- Response],
            io:format("response : ~p~n", [Response]),
            io:format("mochi response : ~p~n", [MochiResponse]),
            io:format("encoded response : ~p~n", [mochijson2:encode(lists:reverse(MochiResponse))]),

            gen_tcp:send(Socket, mochijson2:encode(lists:reverse(MochiResponse))),
            loop(Socket);
        {error, closed} ->
            io:format("connection closed~n"),
            ok
    end.


divide_actions(Actions) ->
    case binary:split(Actions, <<"}{">>) of
        [Action, Tail] -> [<<Action/binary,"}">> | divide_actions(<<"{",Tail/binary>>)];
        Else -> Else
    end.


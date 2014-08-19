-module(hunter_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

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
    spawn(fun() -> loop(Socket, []) end),
    accept(LSocket).

% Echo back whatever data we receive on Socket.
loop(Socket, Players) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            DecodedData = jiffy:decode(Data),

            io:format("data recieved : ~p~n", [Data]),
            io:format("decoded data : ~p~n", [DecodedData]),
            gen_tcp:send(Socket, Data),
            loop(Socket, Players);
        {error, closed} ->
            ok
    end.


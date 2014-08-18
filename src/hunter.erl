-module(hunter).
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%% @spec start() -> ok
%% @doc Start the minemania server.
start() ->
    ensure_started(crypto),
    io:format("trying to start hunter~n"),
    application:start(ranch),
    application:start(cowlib),
    application:start(cowboy),
    application:start(compiler),
    application:start(syntax_tools),
    application:start(goldrush),
    application:start(lager),
    application:start(hunter).


%% @spec stop() -> ok
%% @doc Stop the minemania server.
stop() ->
    application:stop(hunter).
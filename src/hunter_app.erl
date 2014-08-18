-module(hunter_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    io:format("hzhz~n"),
    Dispatch = cowboy_router:compile([  
      {'_', [  
        {"/", cowboy_static, {priv_file, hunter, "index.html"}},%"index.html"}},  
        {"/websocket", hunter_handler, []}  
      ]}  
    ]),
    {ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [{env, [{dispatch, Dispatch}]}]),  
    hunter_sup:start_link().

stop(_State) ->
    ok.

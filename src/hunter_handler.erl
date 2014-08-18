-module(hunter_handler).  
-behaviour(cowboy_http_handler).  
-behaviour(cowboy_websocket_handler).  
-export([init/3, handle/2, terminate/3]).  
-export([  
    websocket_init/3, websocket_handle/3,  
    websocket_info/3, websocket_terminate/3  
]).  
  
init({tcp, http}, _Req, _Opts) ->
    io:format("websocket init~n"),
    {upgrade, protocol, cowboy_websocket}.  
  
  
handle(Req, State) ->  
    io:format("Request not expected: ~p~n", [Req]),  
    {ok, Req2} = cowboy_http_req:reply(404, [{'Content-Type', <<"text/html">>}]),  
    {ok, Req2, State}.  
  
  
websocket_init(_TransportName, Req, _Opts) ->  
    io:format("init websocket"),  
    {ok, Req, undefined_state}.  
  
websocket_handle({text, Msg}, Req, State) ->  
    io:format("Got Data: ~p~n", [Msg]),  
    {reply, {text, << "responding to ", Msg/binary >>}, Req, State, hibernate };  
  
  
websocket_handle(_Any, Req, State) ->
    io:format("wtf?~n"),
    {reply, {text, << "whut?">>}, Req, State, hibernate }.  
  
websocket_info({timeout, _Ref, Msg}, Req, State) ->  
    {reply, {text, Msg}, Req, State};  
  
websocket_info(_Info, Req, State) ->  
    io:format("websocket info"),  
    {ok, Req, State, hibernate}.  
  
websocket_terminate(_Reason, _Req, _State) ->  
    ok.  
  
terminate(_Reason, _Req, _State) ->  
    ok.
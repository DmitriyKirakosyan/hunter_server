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
    spawn(fun() -> loop(Socket, undefined, <<"">>) end),
    accept(LSocket).

% Echo back whatever data we receive on Socket.
loop(Socket, PlayerId, ActionRest) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->

            {Actions, NewActionRest} = parse_client_request(Data, ActionRest),

            Response = process_client_request(Actions),

            BinaryToSend = encode_server_response(Response),

            send_if_not_empty(BinaryToSend, Socket),

            EnsuredPlayerId = if PlayerId =:= undefined ->
                    proplists:get_value(id, lists:last(Actions));
                true -> PlayerId
            end,

            loop(Socket, EnsuredPlayerId, NewActionRest);

        %% Error handling
        {error, closed} when PlayerId =/= undefined ->
            io:format("connection closed from : ~p~n", [PlayerId]),
            hunter_game_controller:logout(PlayerId);
        {error, closed} ->
            io:format("connection closed~n")
    end.

-spec parse_client_request(binary(), binary()) -> {list(), binary()}.
parse_client_request(Request, ActionRest) ->
    io:format("data recieved : ~p~n", [Request]),
    {DividedActions, NewActionRest} = divide_actions(Request, ActionRest),

    io:format("the rest : ~p~n", [NewActionRest]),

    MochiActions = [mochijson2:decode(Item) || Item <- DividedActions],
    Actions = [translate_keys_to_atom(Item) || {struct, Item} <- MochiActions],

    %{struct, Params} = mochijson2:decode(Data),
    io:format("decoded request : ~p~n", [Actions]),

    {Actions, NewActionRest}.

-spec process_client_request(list()) -> list().
process_client_request(Actions) ->
    ResponseList = [hunter_game_controller:action(Item) || Item <- Actions],
    lists:concat(ResponseList).

-spec encode_server_response(list()) -> binary().
encode_server_response(Response) ->
    MochiResponse = [{struct, Item} || Item <- Response],
    io:format("response : ~p~n", [Response]),
    io:format("mochi response : ~p~n", [MochiResponse]),
    %io:format("encoded response : ~p~n", [iolist_to_binary(mochijson2:encode(lists:reverse(MochiResponse)))]),

    EncodedResponse = iolist_to_binary(mochijson2:encode(lists:reverse(MochiResponse))),
    BinaryToSend = <<"#", EncodedResponse/binary, "&">>,
    io:format("binary to send : ~p~n", [BinaryToSend]),
    BinaryToSend.

-spec send_if_not_empty(binary(), gen_tcp:socket()) -> atom().
send_if_not_empty(<<"#[]#">>, _Socket) ->
    ok;
send_if_not_empty(Request, Socket) ->
    gen_tcp:send(Socket, Request).


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

divide_actions(Actions, PrevRest) when is_binary(Actions) ->
    case binary:split(Actions, [?ACTION_TOKEN, <<?ACTION_TOKEN/binary, ?ACTION_TOKEN/binary>>], [global]) of
        [<<"">> | _TailActions] when PrevRest =/= <<"">> ->
            io:format("ERROR! completed first action but there is a prev rest : ~p~n", [PrevRest]),
            {[], <<"">>};
        [<<"">> | TailActions] ->
            divide_actions(TailActions);
        [UncompletedAction | TailActions] ->
            divide_actions([<<PrevRest/binary, UncompletedAction/binary>> | TailActions]);
        [] ->
            [[], <<"">>]
    end;
divide_actions(WrongActions, _PrevRest) ->
    io:format("ERROR! wrong type of actions : ~p, expected type : binary~n", [WrongActions]),
    {error, wrong_actions}.

divide_actions(Actions) when is_list(Actions) ->
    {lists:sublist(Actions, length(Actions)-1), lists:last(Actions)};
divide_actions(WrongActions) ->
    io:format("ERROR! wrong type of actions : ~p, expected type : list~n", [WrongActions]),
    {error, wrong_actions}.


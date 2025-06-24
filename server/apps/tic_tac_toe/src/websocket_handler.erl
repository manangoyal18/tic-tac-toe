-module(websocket_handler).
-behaviour(cowboy_websocket).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

-include("tic_tac_toe.hrl").

init(Req, Opts) ->
    {cowboy_websocket, Req, Opts}.

websocket_init(State) ->
    case game_manager:new_player() of
        {ok, GamePid, Symbol} ->
            gen_server:call(GamePid, {join, self()}),

            JsonSymbol = jsone:encode(#{
                <<"type">> => <<"assign_symbol">>,
                <<"symbol">> => list_to_binary(Symbol)
            }),

            {ok, GameState} = gen_server:call(GamePid, get_state),
            JsonState = jsone:encode(#{
                <<"type">> => <<"game_state">>,
                <<"board">> => GameState#game_state.board,
                <<"turn">> => list_to_binary(GameState#game_state.turn)
            }),

            io:format("[websocket_handler] Assigned symbol ~p and sent initial state~n", [Symbol]),
            {reply, [{text, JsonSymbol}, {text, JsonState}], {GamePid, Symbol}};

        {error, Reason} ->
            error_logger:error_msg("Failed to assign player: ~p", [Reason]),
            {stop, State}
    end.

websocket_handle({text, Msg}, {GamePid, Symbol} = State) ->
    try
        Data = jsone:decode(Msg),
        case maps:get(<<"type">>, Data) of
            <<"move">> ->
                Row = maps:get(<<"row">>, Data),
                Col = maps:get(<<"col">>, Data),
                %io:format("[websocket_handler] Move received: ~p by ~p at ~p,~p~n", [Symbol, Row, Col]),
                io:format("[websocket_handler] Move received: symbol=~p, row=~p, col=~p~n", [Symbol, Row, Col]),

                case gen_server:call(GamePid, {make_move, Row, Col, Symbol}) of
                    ok -> ok;
                    {error, Reason} -> io:format("[websocket_handler] Invalid move: ~p~n", [Reason]);
                    {game_over, Result} -> io:format("[websocket_handler] Game over: ~p~n", [Result])
                end,
                {ok, State};

            <<"reset">> ->
                io:format("[websocket_handler] Reset requested~n"),
                gen_server:cast(GamePid, reset),
                {ok, State}
        end
    catch _:_ ->
        io:format("[websocket_handler] Invalid message format: ~p~n", [Msg]),
        {ok, State}
    end;

websocket_handle(_Data, State) ->
    {ok, State}.

websocket_info({send, Msg}, State) ->
    io:format("[websocket_handler] Sending message to client: ~p~n", [Msg]),
    {reply, {text, Msg}, State};

websocket_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _PartialReq, {GamePid, _Symbol}) ->
    gen_server:cast(GamePid, {player_disconnected, self()}),
    ok.

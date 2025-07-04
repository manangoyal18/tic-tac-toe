-module(websocket_handler).
-behaviour(cowboy_websocket).

-export([init/2]).
-export([websocket_init/3]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

-include("tic_tac_toe.hrl").  

init(Req, Opts) ->
    {cowboy_websocket, Req, Opts}.

websocket_init(_TransportName, _Req, _Opts) ->
    case game_manager:new_player() of
        {ok, GamePid, Symbol} ->
            gen_server:call(GamePid, {join, self()}),

            JsonSymbol = jsone:encode(#{
                <<"type">> => <<"assign_symbol">>,
                <<"symbol">> => Symbol  % Already a binary
            }),

            {ok, GameState} = gen_server:call(GamePid, get_state),
            JsonState = jsone:encode(#{
                <<"type">> => <<"game_state">>,
                <<"board">> => GameState#game_state.board,
                <<"turn">> => GameState#game_state.turn  % Already a binary
            }),

            io:format("[websocket_handler] Assigned symbol ~p and sent initial state~n", [Symbol]),
            {[], #{idle_timeout => infinity}, {GamePid, Symbol}, [ {text, JsonSymbol}, {text, JsonState} ]}; 

            
        {error, Reason} ->
            error_logger:error_msg("Failed to assign player: ~p", [Reason]),
            {stop, normal,undefined}
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

terminate(_Reason, _PartialReq, _State = {GamePid, _Symbol}) when is_pid(GamePid) ->
    io:format("Terminating connection for ~p~n", [GamePid]),
    gen_server:cast(GamePid, {player_disconnected, self()}),
    ok;

terminate(Reason, _PartialReq, State) ->
    io:format("Unknown terminate state: ~p ~p~n", [Reason, State]),
    ok.

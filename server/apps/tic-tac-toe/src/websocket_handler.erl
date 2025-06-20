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
    % Register with game manager to get a game and symbol
    case game_manager:new_player() of
        {ok, GamePid, Symbol} ->
            % Join the game session
            gen_server:call(GamePid, {join, self()}),
            
            % Send initial symbol assignment to client
            JsonSymbol = jsone:encode(#{
                type => "assign_symbol",
                symbol => Symbol
            }),
            
            % Get current game state
            {ok, GameState} = gen_server:call(GamePid, get_state),
            JsonState = jsone:encode(#{
                type => "game_state",
                board => GameState#game_state.board,
                turn => GameState#game_state.turn
            }),
            
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
                case gen_server:call(GamePid, {make_move, Row, Col, Symbol}) of
                    ok -> ok;
                    {error, Reason} -> 
                        error_logger:error_msg("Invalid move: ~p", [Reason]);
                    {game_over, _} ->
                        % Game over, we'll get notified via websocket_info
                        ok
                end,
                {ok, State};        
            <<"reset">> ->
                % For simplicity, we'll just disconnect and let the client reconnect
                % In a real app, you'd implement proper game reset logic
                  % {stop, normal, State}
                    gen_server:cast(GamePid, reset),
                    {ok, State}
        end
        %{ok, State}
    catch _:_ ->
        error_logger:error_msg("Invalid message format: ~p", [Msg]),
        {ok, State}
    end;

websocket_handle(_Data, State) ->
    {ok, State}.

websocket_info({send, Msg}, State) ->
    {reply, {text, Msg}, State};

websocket_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _PartialReq, {GamePid, _Symbol}) ->
    % Notify game session that we're disconnecting
    gen_server:cast(GamePid, {player_disconnected, self()}),
    ok.
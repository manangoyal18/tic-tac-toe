-module(websocket_handler).

%-behaviour(cowboy_websocket).

-include("../include/tic_tac_toe.hrl").

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2, terminate/3]).

init(Req, _State) ->
    GameId = cowboy_req:binding(game_id, Req),
    {cowboy_websocket, Req, #{game_id => GameId}}.

websocket_init(State = #{game_id := GameId}) ->
    case tic_tac_toe_server:join_game(GameId) of
        {ok, Game} ->
            Response = game_to_json(Game),
            {reply, {text, jsx:encode(#{type => ?WS_MSG_JOIN, data => Response})}, State};
        {error, _Reason} ->
            {reply, {text, jsx:encode(#{type => ?WS_MSG_ERROR, error => <<"join_failed">>})}, State}
    end.

websocket_handle({text, Msg}, State) ->
    Data = jsx:decode(Msg, [return_maps]),
    handle_ws_message(Data, State);
websocket_handle(_Data, State) ->
    {ok, State}.

websocket_info({game_update, Json}, State) ->
    {reply, {text, Json}, State};
websocket_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _Req, _State) ->
    ok.

handle_ws_message(#{<<"type">> := ?WS_MSG_MOVE,
                    <<"player">> := Player,
                    <<"row">> := Row,
                    <<"col">> := Col},
                  State = #{game_id := GameId}) ->
    case tic_tac_toe_server:make_move(GameId, Player, Row, Col) of
        {ok, _Game} ->
            {ok, State};
        {error, Reason} ->
            ErrorMsg = jsx:encode(#{type => ?WS_MSG_ERROR, error => atom_to_binary(Reason, utf8)}),
            {reply, {text, ErrorMsg}, State}
    end;
handle_ws_message(_Msg, State) ->
    {ok, State}.

game_to_json(#game_response{} = Game) ->
    #{game_id => Game#game_response.game_id,
      board => Game#game_response.board,
      current_turn => Game#game_response.current_turn,
      status => Game#game_response.status,
      winner => Game#game_response.winner,
      score_x => Game#game_response.score_x,
      score_o => Game#game_response.score_o,
      ties => Game#game_response.ties,
      player_symbol => Game#game_response.player_symbol}.

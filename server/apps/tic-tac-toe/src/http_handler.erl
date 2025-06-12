-module(http_handler).

%-behaviour(cowboy_handler).

-include("../include/tic_tac_toe.hrl").

-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    Path = cowboy_req:path(Req0),
    handle_request(Method, Path, Req0, State).

handle_request(<<"POST">>, <<"/api/games">>, Req0, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    case jsx:decode(Body, [return_maps]) of
        #{<<"type">> := <<"single_player">>} ->
            {ok, GameId} = tic_tac_toe_server:create_game(),
            {ok, Game} = tic_tac_toe_server:join_game(GameId),
            Response = game_to_json(Game),
            Req2 =
                cowboy_req:reply(200,
                                 #{<<"content-type">> => <<"application/json">>},
                                 jsx:encode(Response),
                                 Req1),
            {ok, Req2, State};
        #{<<"type">> := <<"multi_player">>} ->
            {ok, GameId} = tic_tac_toe_server:create_game(),
            Response = #{game_id => GameId, status => <<"waiting">>},
            Req2 =
                cowboy_req:reply(200,
                                 #{<<"content-type">> => <<"application/json">>},
                                 jsx:encode(Response),
                                 Req1),
            {ok, Req2, State};
        _ ->
            Req2 =
                cowboy_req:reply(400,
                                 #{<<"content-type">> => <<"application/json">>},
                                 jsx:encode(#{error => <<"invalid_request">>}),
                                 Req1),
            {ok, Req2, State}
    end;
handle_request(<<"POST">>, <<"/api/games/", GameId/binary>>, Req0, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    case jsx:decode(Body, [return_maps]) of
        #{<<"action">> := <<"join">>} ->
            case tic_tac_toe_server:join_game(GameId) of
                {ok, Game} ->
                    Response = game_to_json(Game),
                    Req2 =
                        cowboy_req:reply(200,
                                         #{<<"content-type">> => <<"application/json">>},
                                         jsx:encode(Response),
                                         Req1);
                {error, Reason} ->
                    Response = #{error => atom_to_binary(Reason, utf8)},
                    Req2 =
                        cowboy_req:reply(400,
                                         #{<<"content-type">> => <<"application/json">>},
                                         jsx:encode(Response),
                                         Req1)
            end,
            {ok, Req2, State};
        _ ->
            Req2 =
                cowboy_req:reply(400,
                                 #{<<"content-type">> => <<"application/json">>},
                                 jsx:encode(#{error => <<"invalid_request">>}),
                                 Req1),
            {ok, Req2, State}
    end;
handle_request(<<"POST">>, Path, Req0, State) ->
    case binary:split(Path, <<"/">>, [global]) of
        [<<>>, <<"api">>, <<"games">>, GameId, <<"move">>] ->
            {ok, Body, Req1} = cowboy_req:read_body(Req0),
            case jsx:decode(Body, [return_maps]) of
                #{<<"player">> := Player,
                  <<"row">> := Row,
                  <<"col">> := Col} ->
                    case tic_tac_toe_server:make_move(GameId, Player, Row, Col) of
                        {ok, Game} ->
                            Response = game_to_json(Game),
                            Req2 =
                                cowboy_req:reply(200,
                                                 #{<<"content-type">> => <<"application/json">>},
                                                 jsx:encode(Response),
                                                 Req1);
                        {error, Reason} ->
                            Response = #{error => atom_to_binary(Reason, utf8)},
                            Req2 =
                                cowboy_req:reply(400,
                                                 #{<<"content-type">> => <<"application/json">>},
                                                 jsx:encode(Response),
                                                 Req1)
                    end,
                    {ok, Req2, State};
                _ ->
                    Req2 =
                        cowboy_req:reply(400,
                                         #{<<"content-type">> => <<"application/json">>},
                                         jsx:encode(#{error => <<"invalid_request">>}),
                                         Req1),
                    {ok, Req2, State}
            end;
        _ ->
            Req1 =
                cowboy_req:reply(404,
                                 #{<<"content-type">> => <<"text/plain">>},
                                 <<"Not found">>,
                                 Req0),
            {ok, Req1, State}
    end;
handle_request(<<"GET">>, <<"/api/games/", GameId/binary>>, Req0, State) ->
    case tic_tac_toe_server:get_game_state(GameId) of
        {ok, Game} ->
            Response = game_to_json(Game),
            Req1 =
                cowboy_req:reply(200,
                                 #{<<"content-type">> => <<"application/json">>},
                                 jsx:encode(Response),
                                 Req0);
        {error, Reason} ->
            Response = #{error => atom_to_binary(Reason, utf8)},
            Req1 =
                cowboy_req:reply(404,
                                 #{<<"content-type">> => <<"application/json">>},
                                 jsx:encode(Response),
                                 Req0)
    end,
    {ok, Req1, State};
handle_request(_, _, Req0, State) ->
    Req1 =
        cowboy_req:reply(404,
                         #{<<"content-type">> => <<"application/json">>},
                         jsx:encode(#{error => <<"not_found">>}),
                         Req0),
    {ok, Req1, State}.

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

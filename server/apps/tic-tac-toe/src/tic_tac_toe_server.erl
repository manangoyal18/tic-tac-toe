-module(tic_tac_toe_server).

-behaviour(gen_server).

-include("../include/tic_tac_toe.hrl").

%% API
-export([start_link/0]).
-export([create_game/0, join_game/1, make_move/4, get_game_state/1]).
-export([get_all_games/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {games = #{} :: map()}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

create_game() ->
    gen_server:call(?SERVER, {create_game}).

join_game(GameId) ->
    gen_server:call(?SERVER, {join_game, GameId}).

make_move(GameId, Player, Row, Col) ->
    gen_server:call(?SERVER, {make_move, GameId, Player, Row, Col}).

get_game_state(GameId) ->
    gen_server:call(?SERVER, {get_game_state, GameId}).

get_all_games() ->
    gen_server:call(?SERVER, get_all_games).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{games = #{}}}.

handle_call({create_game}, _From, State) ->
    GameId =
        uuid:uuid_to_string(
            uuid:get_v4()),
    Game = #game{id = GameId},
    GamesMap = State#state.games,
    NewGames = GamesMap#{GameId => Game},

    %NewGames = State#state.games#{GameId => Game},
    {reply, {ok, GameId}, State#state{games = NewGames}};
handle_call({join_game, GameId}, {From, _}, State) ->
    case maps:get(GameId, State#state.games, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        Game = #game{player_x = undefined} ->
            NewGame = Game#game{player_x = From, status = <<"waiting">>},
            UpdatedGames = (State#state.games)#{GameId => NewGame},

            % UpdatedGames = State#state.games#{GameId => NewGame},
            {reply, {ok, player_assignment(<<"X">>, NewGame)}, State#state{games = UpdatedGames}};
        Game = #game{player_o = undefined} ->
            NewGame = Game#game{player_o = From, status = <<"playing">>},
            UpdatedGames = (State#state.games)#{GameId => NewGame},

            %UpdatedGames = State#state.games#{GameId => NewGame},
            {reply, {ok, player_assignment(<<"O">>, NewGame)}, State#state{games = UpdatedGames}};
        _ ->
            {reply, {error, game_full}, State}
    end;
handle_call({make_move, GameId, Player, Row, Col}, {From, _}, State) ->
    case maps:get(GameId, State#state.games, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        Game ->
            case validate_move(Game, Player, Row, Col, From) of
                {ok, UpdatedGame} ->
                    {_Winner, FinalGame} = check_winner(UpdatedGame),
                    UpdatedGames = (State#state.games)#{GameId => FinalGame},

                    %UpdatedGames = State#state.games#{GameId => FinalGame},
                    broadcast_update(FinalGame),
                    {reply, {ok, game_to_response(FinalGame)}, State#state{games = UpdatedGames}};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end
    end;
handle_call({get_game_state, GameId}, _From, State) ->
    case maps:get(GameId, State#state.games, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        Game ->
            {reply, {ok, game_to_response(Game)}, State}
    end;
handle_call(get_all_games, _From, State) ->
    Games = maps:map(fun(_K, V) -> game_to_response(V) end, State#state.games),
    {reply, {ok, maps:values(Games)}, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

validate_move(Game, Player, Row, Col, From) ->
    #game{current_turn = CurrentTurn,
          player_x = PlayerX,
          player_o = PlayerO,
          board = Board,
          status = Status} =
        Game,

    case Status of
        <<"playing">> ->
            case Player =:= CurrentTurn of
                true ->
                    case Player =:= <<"X">> andalso From =/= PlayerX
                         orelse Player =:= <<"O">> andalso From =/= PlayerO
                    of
                        true ->
                            {error, invalid_player};
                        false ->
                            case Row < 0 orelse Row > 2 orelse Col < 0 orelse Col > 2 of
                                true ->
                                    {error, invalid_position};
                                false ->
                                    Cell = lists:nth(Col + 1, lists:nth(Row + 1, Board)),
                                    case Cell of
                                        <<>> ->
                                            NewBoard = set_board_position(Board, Row, Col, Player),
                                            NewGame =
                                                Game#game{board = NewBoard,
                                                          current_turn = next_turn(CurrentTurn)},
                                            {ok, NewGame};
                                        _ ->
                                            {error, position_taken}
                                    end
                            end
                    end;
                false ->
                    {error, not_your_turn}
            end;
        _ ->
            {error, game_not_started}
    end.

set_board_position(Board, Row, Col, Value) ->
    RowList = lists:nth(Row + 1, Board),
    NewRowList = set_list_position(RowList, Col, Value),
    set_list_position(Board, Row, NewRowList).

set_list_position(List, Index, Value) ->
    {Before, [_ | After]} = lists:split(Index, List),
    Before ++ [Value | After].

next_turn(<<"X">>) ->
    <<"O">>;
next_turn(<<"O">>) ->
    <<"X">>.

check_winner(Game = #game{board = Board}) ->
    case check_win_conditions(Board) of
        <<"X">> ->
            UpdatedGame =
                Game#game{status = <<"finished">>,
                          winner = <<"X">>,
                          score_x = Game#game.score_x + 1},
            {<<"X">>, UpdatedGame};
        <<"O">> ->
            UpdatedGame =
                Game#game{status = <<"finished">>,
                          winner = <<"O">>,
                          score_o = Game#game.score_o + 1},
            {<<"O">>, UpdatedGame};
        tie ->
            UpdatedGame =
                Game#game{status = <<"finished">>,
                          winner = <<"tie">>,
                          ties = Game#game.ties + 1},
            {tie, UpdatedGame};
        false ->
            {false, Game}
    end.

check_win_conditions(Board) ->
    % Check rows
    case lists:any(fun(Row) -> all_equal(Row) end, Board) of
        true ->
            hd(hd(Board));
        false ->
            % Check columns
            case lists:any(fun(Col) ->
                              all_equal([lists:nth(Col, lists:nth(Row, Board))
                                         || Row <- lists:seq(1, 3)])
                           end,
                           lists:seq(1, 3))
            of
                true ->
                    hd([lists:nth(Col, hd(Board)) || Col <- lists:seq(1, 3)]);
                false ->
                    % Check diagonals
                    Diagonal1 = [lists:nth(I, lists:nth(I, Board)) || I <- lists:seq(1, 3)],
                    Diagonal2 = [lists:nth(4 - I, lists:nth(I, Board)) || I <- lists:seq(1, 3)],
                    case all_equal(Diagonal1) of
                        true ->
                            hd(Diagonal1);
                        false ->
                            case all_equal(Diagonal2) of
                                true ->
                                    hd(Diagonal2);
                                false ->
                                    % Check for tie
                                    case lists:all(fun(Row) ->
                                                      lists:all(fun(Cell) -> Cell =/= <<>> end, Row)
                                                   end,
                                                   Board)
                                    of
                                        true ->
                                            tie;
                                        false ->
                                            false
                                    end
                            end
                    end
            end
    end.

all_equal([H | T]) when H =/= <<>> ->
    lists:all(fun(X) -> X =:= H end, T);
all_equal(_) ->
    false.

broadcast_update(Game = #game{player_x = PidX, player_o = PidO}) ->
    Response = game_to_response(Game),
    Json = jsx:encode(Response),
    case PidX of
        undefined ->
            ok;
        _ when is_pid(PidX) ->
            PidX ! {game_update, Json}
    end,
    case PidO of
        undefined ->
            ok;
        _ when is_pid(PidO) ->
            PidO ! {game_update, Json}
    end.

game_to_response(#game{id = Id,
                       board = Board,
                       current_turn = CurrentTurn,
                       status = Status,
                       winner = Winner,
                       score_x = ScoreX,
                       score_o = ScoreO,
                       ties = Ties,
                       player_x = PlayerX,
                       player_o = PlayerO}) ->
    #game_response{game_id = Id,
                   board = Board,
                   current_turn = CurrentTurn,
                   status = Status,
                   winner = Winner,
                   score_x = ScoreX,
                   score_o = ScoreO,
                   ties = Ties,
                   player_symbol =
                       case self() of
                           PlayerX ->
                               <<"X">>;
                           PlayerO ->
                               <<"O">>;
                           _ ->
                               <<>>
                       end}.

player_assignment(Symbol, #game{} = Game) ->
    Response = game_to_response(Game),
    Response#game_response{player_symbol = Symbol}.

%% src/game_session.erl
-module(game_session).
-behaviour(gen_server).

-include("tic_tac_toe.hrl").

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([print_board/1, notify_reset/1]).

% Client API
start_link() ->
    gen_server:start_link(?MODULE, [], []).

% Server callbacks
init([]) ->
    InitialState = #game_state{
        %board = [["", "", ""], ["", "", ""], ["", "", ""]],
        board = [["empty", "empty", "empty"],
                 ["empty", "empty", "empty"],
                 ["empty", "empty", "empty"]],
        turn = "x",
        player_x = undefined,
        player_o = undefined
    },
    {ok, InitialState}.

handle_call({join, PlayerPid}, _From, State = #game_state{player_x = undefined}) ->
    % First player joins as X
    erlang:monitor(process, PlayerPid),
    {reply, ok, State#game_state{player_x = PlayerPid}};

handle_call({join, PlayerPid}, _From, State = #game_state{player_o = undefined}) ->
    % Second player joins as O
    erlang:monitor(process, PlayerPid),
    {reply, ok, State#game_state{player_o = PlayerPid}};

handle_call({make_move, Row, Col, Symbol}, _From, State = #game_state{turn = Turn}) ->
    % Validate move
    if 
        Symbol =/= Turn ->
            {reply, {error, not_your_turn}, State};
        Row < 0 orelse Row > 2 orelse Col < 0 orelse Col > 2 ->
            {reply, {error, invalid_position}, State};
        true ->
            case make_move_if_valid(Row, Col, Symbol, State) of
                {ok, NewState} ->
                    % Check game status after move
                    case check_game_result(NewState#game_state.board) of
                        ongoing ->
                            % Notify both players of new state
                            notify_players(NewState),
                            {reply, ok, NewState};
                        Result ->
                            % Game over, notify players
                            notify_game_over(NewState, Result),
                            {reply, {game_over, Result}, NewState}
                    end;
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end
    end;

handle_call(get_state, _From, State) ->
    {reply, {ok, State}, State};

handle_call(info, _From, State) ->
    io:format("Current board:~n"),
    print_board(State#game_state.board),
    io:format("Turn: ~p~n", [State#game_state.turn]),
    io:format("Player X: ~p~n", [State#game_state.player_x]),
    io:format("Player O: ~p~n", [State#game_state.player_o]),
    {reply, ok, State}.


%handle_cast(_Msg, State) ->
%    {noreply, State}.

handle_cast(reset, State) ->
   % io:format("[game_session] Game reset requested~n"),
    notify_reset(State), 
    NewState = #game_state{
        %board = [["", "", ""], ["", "", ""], ["", "", ""]],
        %board = [["empty", "empty", "empty"],["empty", "empty", "empty"],["empty", "empty", "empty"]],
        %board = [[<<"">>, <<"">>, <<"">>], [<<"">>, <<"">>, <<"">>], [<<"">>, <<"">>, <<"">>]],
         board = [[<<>>, <<>>, <<>>], [<<>>, <<>>, <<>>], [<<>>, <<>>, <<>>]],

        turn = "x",
        player_x = undefined,
        player_o = undefined
    },
    
    {noreply, NewState};

handle_cast({player_disconnected, Pid}, State = #game_state{player_x = PidX, player_o = PidO}) ->
    NewState = case Pid of
        PidX -> State#game_state{player_x = undefined};
        PidO -> State#game_state{player_o = undefined};
        _ -> State
    end,
    {noreply, NewState}.



%handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
%    % Handle player disconnection
%    NewState = case State of
%        #game_state{player_x = Pid} -> State#game_state{player_x = undefined};
%        #game_state{player_o = Pid} -> State#game_state{player_o = undefined};
%        _ -> State
%    end,
%    {noreply, NewState};

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    % Identify and remove the disconnected player
    NewState = case State of
        #game_state{player_x = Pid} -> State#game_state{player_x = undefined};
        #game_state{player_o = Pid} -> State#game_state{player_o = undefined};
        _ -> State
    end,

    % Notify remaining player of disconnection
    OtherPid = case {State#game_state.player_x, State#game_state.player_o} of
        {Pid, Other} when is_pid(Other) -> Other;
        {Other, Pid} when is_pid(Other) -> Other;
        _ -> undefined
    end,

    case is_pid(OtherPid) of
        true ->
            Msg = jsone:encode(#{
                <<"type">> => <<"game_result">>,
                <<"result">> => <<"Opponent disconnected! You win!">>
            }),
            OtherPid ! {send, Msg};
        false -> ok
    end,

    {noreply, NewState}.


%handle_info(_Info, State) ->
 %   {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% Helper functions
make_move_if_valid(Row, Col, Symbol, State = #game_state{board = Board}) ->
   % CurrentCell = lists:nth(Row+1, lists:nth(Col+1, Board)),
    CurrentRow = lists:nth(Row + 1, Board),
    CurrentCell = lists:nth(Col + 1, CurrentRow),
    if
        CurrentCell =/= <<>> ->
            {error, cell_occupied};
        true ->
            % Update the board
            %NewRow = lists:sublist(Board, Row) ++ 
            %         [set_element(Col+1, lists:nth(Row+1, Board), Symbol)] ++ 
            %         lists:nthtail(Row+1, Board),
            %NewBoard = lists:sublist(Board, Row) ++ [NewRow] ++ lists:nthtail(Row+1, Board),
           % OldRow = lists:nth(Row + 1, Board),
           % UpdatedRow = set_element(Col + 1, OldRow, Symbol),
           % NewBoard = set_element(Row + 1, Board, UpdatedRow),
            UpdatedRow = set_element(Col + 1, CurrentRow, Symbol),
            NewBoard = set_element(Row + 1, Board, UpdatedRow),

        io:format("Checking cell ~p at ~p,~p~n", [CurrentCell, Row, Col]),

            NewTurn = case Symbol of "x" -> "o"; "o" -> "x" end,
            {ok, State#game_state{board = NewBoard, turn = NewTurn}}
    end.

set_element(1, [_|T], X) -> [X|T];
set_element(N, [H|T], X) -> [H|set_element(N-1, T, X)].

check_game_result(Board) ->
    % Check rows
    case check_lines(Board) of
        {win, Symbol} -> 
            case Symbol of "x" -> x_wins; "o" -> o_wins end;
        no_win ->
            % Check columns
            Columns = [[lists:nth(I, lists:nth(1, Board)), 
                       lists:nth(I, lists:nth(2, Board)), 
                       lists:nth(I, lists:nth(3, Board))] || I <- lists:seq(1, 3)],
            case check_lines(Columns) of
                {win, Symbol} -> 
                    case Symbol of "x" -> x_wins; "o" -> o_wins end;
                no_win ->
                    % Check diagonals
                    Diag1 = [lists:nth(1, lists:nth(1, Board)), 
                             lists:nth(2, lists:nth(2, Board)), 
                             lists:nth(3, lists:nth(3, Board))],
                    Diag2 = [lists:nth(3, lists:nth(1, Board)), 
                             lists:nth(2, lists:nth(2, Board)), 
                             lists:nth(1, lists:nth(3, Board))],
                    case check_lines([Diag1, Diag2]) of
                        {win, Symbol} -> 
                            case Symbol of "x" -> x_wins; "o" -> o_wins end;
                        no_win ->
                            % Check for draw
                            case is_board_full(Board) of
                                true -> draw;
                                false -> ongoing
                            end
                    end
            end
    end.

check_lines(Lines) ->
    case lists:filter(fun(Line) -> length(Line) == 3 andalso hd(Line) =/= "" andalso lists:all(fun(X) -> X == hd(Line) end, Line) end, Lines) of
        [] -> no_win;
        [Winner|_] -> {win, hd(Winner)}
    end.

is_board_full(Board) ->
    lists:all(fun(Row) -> lists:all(fun(Cell) -> Cell =/= "" end, Row) end, Board).

notify_players(State = #game_state{player_x = PidX, player_o = PidO}) ->
    JsonState = jsone:encode(#{
        <<"type">> => <<"game_state">>,
        <<"board">> => State#game_state.board,
        <<"turn">> => list_to_binary(State#game_state.turn)
    }),
    io:format("[game_session] Notifying players of new state: ~p~n", [JsonState]),
    if is_pid(PidX) -> PidX ! {send, JsonState}; true -> ok end,
    if is_pid(PidO) -> PidO ! {send, JsonState}; true -> ok end.

notify_game_over(State, Result) ->
    ResultStr = case Result of
        x_wins -> "X wins!";
        o_wins -> "O wins!";
        draw -> "It's a draw!"
    end,
    JsonResult = jsone:encode(#{
        <<"type">> => <<"game_result">>,
        <<"result">> => list_to_binary(ResultStr)
    }),
    io:format("[game_session] Game result: ~s~n", [ResultStr]),
    if is_pid(State#game_state.player_x) -> State#game_state.player_x ! {send, JsonResult}; true -> ok end,
    if is_pid(State#game_state.player_o) -> State#game_state.player_o ! {send, JsonResult}; true -> ok end.

print_board(Board) ->
    lists:foreach(fun(Row) ->
        RowStr = lists:join(" | ", Row),
        io:format("~s~n", [RowStr])
    end, Board).

notify_reset(#game_state{player_x = PidX, player_o = PidO}) ->
    Msg = jsone:encode(#{
        <<"type">> => <<"game_result">>,
        <<"result">> => <<"Game has been reset">>
    }),
    io:format("[game_session] Notifying reset~n"),
    % For simplicity we don't send to specific PIDs here because reset clears players
    if is_pid(PidX) -> PidX ! {send, Msg}; true -> ok end,
    if is_pid(PidO) -> PidO ! {send, Msg}; true -> ok end.
    


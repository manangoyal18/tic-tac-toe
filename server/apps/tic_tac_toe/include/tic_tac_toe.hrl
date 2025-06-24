%% include/tic_tac_toe.hrl
-record(game_state, {
    board :: [[string()]],  % 3x3 grid
    turn :: string(),       % "x" or "o"
    player_x :: pid(),      % Pid of player X
    player_o :: pid()       % Pid of player O
}).

-type game_result() :: x_wins | o_wins | draw | ongoing.
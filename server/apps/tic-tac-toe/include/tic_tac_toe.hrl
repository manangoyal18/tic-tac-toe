%%% Record definitions for game state
-record(game,
        {id :: binary(),
         player_x :: pid() | undefined,
         player_o :: pid() | undefined,
         board = [[<<>>, <<>>, <<>>], [<<>>, <<>>, <<>>], [<<>>, <<>>, <<>>]] :: [[binary()]],
         current_turn = <<"X">> :: binary(),
         status = <<"waiting">> :: binary(), % <<"waiting">>, <<"playing">>, <<"finished">>
         winner = <<>> :: binary(),
         score_x = 0 :: integer(),
         score_o = 0 :: integer(),
         ties = 0 :: integer()}).
%%% API response records
-record(game_response,
        {game_id :: binary(),
         board :: [[binary()]],
         current_turn :: binary(),
         status :: binary(),
         winner :: binary(),
         score_x :: integer(),
         score_o :: integer(),
         ties :: integer(),
         player_symbol :: binary()}).

%%% WebSocket message types
-define(WS_MSG_JOIN, <<"join">>).
-define(WS_MSG_MOVE, <<"move">>).
-define(WS_MSG_UPDATE, <<"update">>).
-define(WS_MSG_ERROR, <<"error">>).

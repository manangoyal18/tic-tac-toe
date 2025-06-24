%% src/game_manager.erl
-module(game_manager).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([new_player/0, make_move/4]).

% Client API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

new_player() ->
    gen_server:call(?MODULE, new_player).

make_move(GamePid, Row, Col, Symbol) ->
    gen_server:call(GamePid, {make_move, Row, Col, Symbol}).

% Server callbacks
init([]) ->
    {ok, #{}}.

handle_call(new_player, _From, State) ->
    % Find a game waiting for a player or start a new one
    case find_waiting_game(State) of
        {ok, GamePid} ->
            % Assign player to existing game
            {reply, {ok, GamePid, "o"}, State};
        none ->
            % Start new game and assign as player X
            {ok, GamePid} = game_session:start_link(),
            {reply, {ok, GamePid, "x"}, State#{GamePid => waiting}}
    end;

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% Helper functions
find_waiting_game(Games) ->
    case lists:filter(fun({_Pid, Status}) -> Status == waiting end, maps:to_list(Games)) of
        [{GamePid, _}] -> {ok, GamePid};
        [] -> none
    end.
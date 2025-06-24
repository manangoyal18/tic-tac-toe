%% src/tic_tac_toe_app.erl
-module(tic_tac_toe_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    % Read port from config
    {ok, Port} = application:get_env(tic_tac_toe, websocket_port),
    
    % Start Cowboy web server with WebSocket handler
    %   Dispatch = cowboy_router:compile([
    %       {'_', [
    %           {"/", cowboy_static, {priv_file, tic_tac_toe, "index.html"}},
    %           {"/ws", websocket_handler, []}
    %       ]}
    %   ]),
    Dispatch = cowboy_router:compile([
    {'_', [
        {"/websocket", websocket_handler, []}
    ]}
    ]),
    
    %{ok, _} = cowboy:start_clear(
    %    tic_tac_toe_http_listener,
    %    [{port, Port}],
    %    #{env => #{dispatch => Dispatch}}
    %),
    {ok, _} = cowboy:start_clear(tic_tac_toe_http_listener, [{port, Port}, {ip, {0,0,0,0}}], #{
    env => #{dispatch => Dispatch}
}),
    


    % Start game manager supervisor
    tic_tac_toe_sup:start_link().

stop(_State) ->
    ok = cowboy:stop_listener(tic_tac_toe_http_listener).
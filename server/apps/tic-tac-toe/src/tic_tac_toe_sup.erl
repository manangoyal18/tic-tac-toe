-module(tic_tac_toe_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags =
        #{strategy => one_for_one,
          intensity => 1,
          period => 5},

    ChildSpecs =
        [#{id => tic_tac_toe_server,
           start => {tic_tac_toe_server, start_link, []},
           restart => permanent,
           shutdown => 5000,
           type => worker,
           modules => [tic_tac_toe_server]},
         #{id => cowboy_http,
           start =>
               {cowboy,
                start_clear,
                [http_listener,
                 100,
                 #{env => #{dispatch => dispatch_rules()}},
                 #{stream_handlers => [cowboy_compress_h, cowboy_stream_h]}]},
           restart => permanent,
           shutdown => infinity,
           type => supervisor,
           modules => [cowboy]}],

    {ok, {SupFlags, ChildSpecs}}.

dispatch_rules() ->
    cowboy_router:compile([{'_',
                            [{"/api/games", tic_tac_toe_http_handler, []},
                             {"/api/games/:game_id", http_handler, []},
                             {"/api/games/:game_id/move", tic_tac_toe_http_handler, []},
                             {"/ws/games/:game_id", websocket_handler, []}]}]).

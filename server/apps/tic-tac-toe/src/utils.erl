%% src/utils.erl
-module(utils).
-export([log/2]).

log(Level, Message) ->
    Timestamp = calendar:local_time(),
    io:format("[~p] [~p] ~p~n", [Timestamp, Level, Message]).
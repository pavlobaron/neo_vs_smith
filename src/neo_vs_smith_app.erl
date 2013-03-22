-module(neo_vs_smith_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    start(ok, ok).

start(_StartType, _StartArgs) ->
    erlang:display("Neo vs. Smiths, the Erlang version."),
    neo_vs_smith_sup:start_link().

stop(_State) ->
    ok.

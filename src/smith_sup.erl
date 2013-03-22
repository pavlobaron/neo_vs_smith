-module(smith_sup).

-behaviour(supervisor).

%% API
-export([start_link/1, clone/1, current/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(_MatrixPid) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

spec() ->
    ?CHILD(smith_clone_srv, worker).

current() ->
    length(supervisor:which_children(smith_sup)).

clone(N) ->
    Len = current(),
    ToStart = case Len of
                  0 -> N;
                  _ -> N - Len
              end,
    clone_int(ToStart).

clone_int(0) ->
    ok;
clone_int(N) ->
    supervisor:start_child(?MODULE, []),
    clone_int(N - 1).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, {{simple_one_for_one, 1000, 1}, [spec()]}}.

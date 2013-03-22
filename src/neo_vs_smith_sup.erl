-module(neo_vs_smith_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, [self()]}, temporary, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    erlang:display("Matrix: ready"),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    FSM = ?CHILD(matrix_fsm, worker),
    Neo = ?CHILD(neo_srv, worker),
    SmithSup = ?CHILD(smith_sup, supervisor),
    Smith = ?CHILD(smith_srv, supervisor),
    Woman = ?CHILD(woman_srv, worker),
    Jackson = ?CHILD(jackson_srv, worker),
    Trinity = ?CHILD(trinity_srv, worker),
    {ok, {{one_for_one, 5, 10}, [FSM, Neo, Smith, Woman, Jackson, Trinity, SmithSup]}}.

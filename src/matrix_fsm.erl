-module(matrix_fsm).

-behaviour(gen_fsm).

-export([start_link/1, init/1, step/2]).

-export([handle_event/3, handle_sync_event/4, handle_info/3,
         terminate/3, code_change/4]).

start_link(MatrixPid) ->
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, [MatrixPid], []).

init(MatrixPid) ->
    {ok, Screenplay} = application:get_env(screenplay),
    gen_fsm:send_event_after(10000, step),
    {ok, step, {MatrixPid, Screenplay}}.

%% states
step(_, {_, []}) ->
    {stop, "Follow me on Twitter, if you like: @pavlobaron", ok};
step(_, {MatrixPid, [{Role, {Action, Payload, {time, Time}}}|Screenplay]}) ->
    case Action of
        fight -> gen_server:cast(whereis(smith), {Action, Payload, Time});
        _ -> ok
    end,
    gen_server:cast(whereis(Role), {Action, Payload, Time}),
    gen_fsm:send_event_after(Time * 1000, step),
    {next_state, step, {MatrixPid, Screenplay}}.

%% callbacks (dummies)

handle_event(_, _, State) ->
    {noreply, State}.

handle_sync_event(_, _, _, State) ->
    {reply, State}.

handle_info(_, _, State) ->
    {reply, State}.

terminate(_, _, _) ->
    ok.

code_change(_, _, _, State) ->
    {ok, State}.

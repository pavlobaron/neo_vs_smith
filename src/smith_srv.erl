-module(smith_srv).

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

start_link(MatrixPid) ->
    gen_server:start_link({local, smith}, ?MODULE, [MatrixPid], []).

init([MatrixPid]) ->
    erlang:display("Smith: ready"),
    {ok, Frequency} = application:get_env(beating_frequency),
    {ok, MaxAttacking} = application:get_env(max_smiths_attacking),
    {ok, {MatrixPid, Frequency, MaxAttacking}}.

handle_call(eval, _From, State) ->
    {reply, State}.

think(Msg) ->
    erlang:display("Smith thinks: " ++ Msg).

handle_cast({clone, N, _}, State) ->
    erlang:display("Smith creates " ++ integer_to_list(N) ++ " clones of himself"),
    think("for (int i = 1; i < " ++ integer_to_list(N) ++ "; i++) {"),
    smith_sup:clone(N),

    {noreply, State};
handle_cast({say, Msg, _}, State) ->
    erlang:display("Smith says: " ++ Msg),
    think("System.out.println(\"" ++ Msg ++ "\");"),

    {noreply, State};
handle_cast({assimilate, Whom, _}, State) ->
    Msg = case Whom of
              neo ->
                  "Neo";
              jackson ->
                  "Jackson"
          end,
    erlang:display("Smith sticks his hand into " ++ Msg ++ "'s body to assimilate him"),
    think("try { assimilate(neo); } catch (Exception e) {"),

    {noreply, State};
handle_cast({look, How, _}, State) ->
    Msg = atom_to_list(How),
    erlang:display("Smith looks " ++ Msg),
    think("throw new LookingException(\"" ++ Msg ++ "\");"),

    {noreply, State};
handle_cast({squish, fruits, _}, State) ->
    erlang:display("Smith squishes some fruits on the ground"),
    think("if (fruitsAvailable) { squish(fruitsCollection); }"),

    {noreply, State};
handle_cast({crash, bench, _}, State) ->
    erlang:display("Smith crashes over a bench"),
    think("if (benchAvailable) { crash(bench); }"),

    {noreply, State};
handle_cast({pile_up, neo, _}, State) ->
    erlang:display("Smith piles up dozens of his clones on Neo"),
    think("for (int i = 0; i < 24; i++) { pileUp(neo); }"),

    {noreply, State};
handle_cast({fight, _, Time}, State) ->
    {_, Frequency, MaxAttacking} = State,
    random:seed(),
    gen_server:cast(self(), {beat, Frequency, MaxAttacking, Time}),

    {noreply, State};
handle_cast({beat, _Frequency, _MaxAttacking, 0}, State) ->
    {noreply, State};
handle_cast({beat, Frequency, MaxAttacking, Time}, State) ->
    TimeDelta = Time - Frequency,
    Sleep = case TimeDelta < 0 of
                true -> Time;
                _ -> Frequency
            end,

    timer:sleep(Sleep * 1000),
    Smiths = supervisor:which_children(smith_sup),
    Max = random:uniform(MaxAttacking),
    beat(Smiths, Max),
    gen_server:cast(self(), {beat, Frequency, MaxAttacking, Time - Sleep}),
    
    {noreply, State};
handle_cast(_, State) ->
    {noreply, State}.

beat([], _) ->
    ok;
beat(_, 0) ->
    ok;
beat([{_, Pid, _, _}|Smiths], N) ->
    case is_process_alive(Pid) of
        true ->
            gen_server:cast(Pid, {beat}),
            beat(Smiths, N - 1);
        _ ->
            beat(Smiths, N)
    end.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-module(neo_srv).

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

start_link(MatrixPid) ->
    gen_server:start_link({local, neo}, ?MODULE, [MatrixPid], []).

init([MatrixPid]) ->
    erlang:display("Neo: ready"),
    {ok, Frequency} = application:get_env(beating_frequency),
    {ok, {MatrixPid, Frequency}}.

handle_call(eval, _From, State) ->
    {reply, State}.

think(Msg) ->
    erlang:display("Neo thinks: " ++ Msg).

handle_cast({resist, _, _}, State) ->
    erlang:display("Neo resists Smith's assimilation attempt"),
    think("F... you!"),

    {noreply, State};
handle_cast({clean, pole, _}, State) ->
    erlang:display("Neo cleans the pole with the body of a Smith clone"),
    think("Never liked your suit, dude"),

    {noreply, State};
handle_cast({lose, pole, _}, State) ->
    erlang:display("Neo loses the pole"),
    think("F...!"),

    {noreply, State};
handle_cast({play, pole, _}, State) ->
    erlang:display("Neo plays friendly with the pole"),
    think("It's better than this stupid spoon"),

    {noreply, State};
handle_cast({look, realistic, _}, State) ->
    erlang:display("Neo looks around, pure realism in his eyes"),
    think("Time to get the hell out of here, before I get hurt"),

    {noreply, State};
handle_cast({bend, matrix, _}, State) ->
    erlang:display("Neo quickly bends the matrix under his feet"),
    think("Careful, man. Don't forget what happened in the restroom"),

    {noreply, State};
handle_cast({cast_off, smith, _}, State) ->
    erlang:display("Neo casts off a dozen of Smith clones"),
    think("Hey, take me out for dinner before you get close like that"),

    {noreply, State};
handle_cast({fly, Where, _}, State) ->
    Msg = case Where of
              reposition ->
                  think("I believe I can fly, I believe I can touch the sky"),
                  "up to have a better fighting position";
              escape ->
                  think("Make some tea, Trinity"),
                  "away, being very realistic"
          end,
    erlang:display("Neo flies " ++ Msg),

    {noreply, State};
handle_cast({grab, What, _}, State) ->
    Msg = case What of
              pole ->
                  think("Oh, look, a pole"),
                  "a pole";
              smith ->
                  think("You should go to a gym, dude"),
                  "a Smith clone"
          end,
    erlang:display("Neo grabs " ++ Msg),

    {noreply, State};
handle_cast({crash, What, _}, State) ->
    Msg = case What of
              bench ->
                  think("Man, exactly like the other day after the pub"),
                  "over a bench";
              wall ->
                  think("Really, like the other day after the pub"),
                  "into a wall"
          end,
    erlang:display("Neo crashes " ++ Msg),

    {noreply, State};
handle_cast({fight, Weapon, Time}, State) ->
    Msg = case Weapon of
              bare -> "his own body";
              pole -> "a pole";
              smith -> "a Smith clone"
          end,
    erlang:display("Neo fights using " ++ Msg ++ " as weapon"),
    think("Trinity, you've been a naughty girl last night. I liked it."),
    {_, Frequency} = State,
    Smiths = supervisor:which_children(smith_sup),
    random:seed(),
    gen_server:cast(self(), {beat, Frequency, Weapon, Smiths, Time}),

    {noreply, State};
handle_cast({beat, _Frequency, _Weapon, _Smiths, 0}, State) ->
    {noreply, State};
handle_cast({beat, Frequency, Weapon, Smiths, Time}, State) ->
    TimeDelta = Time - Frequency,
    Sleep = case TimeDelta < 0 of
                true -> Time;
                _ -> Frequency
            end,

    timer:sleep(Sleep * 1000),
    WeaponFactor = case Weapon of
                       bare -> 1;
                       pole -> 4;
                       smith -> 2
                   end,
    LenSmiths = length(Smiths),
    CloneFactor = case LenSmiths < 10 of
                      true -> 1;
                      _ -> case LenSmiths < 100 of
                               true -> 2;
                               _ -> case LenSmiths < 1000 of
                                        true -> 4;
                                        _ -> 8
                                    end
                           end
                  end,
    Max = round(((LenSmiths / Frequency) * WeaponFactor) / CloneFactor),
    RestSmiths = case Max > 0 of
                     true ->
                         ToKill = random:uniform(Max),
                         erlang:display("Neo eliminates " ++ integer_to_list(ToKill) ++
                                            " of " ++ integer_to_list(LenSmiths + 1)),
                         think("Aha, another bunch of suckers gone!"),
                         beat(Smiths, ToKill);
                     _ ->
                         think("It's " ++
                               integer_to_list(smith_sup:current() + 1) ++
                               " of them, damn it!"),
                         Smiths
                 end,
    gen_server:cast(self(), {beat, Frequency, Weapon, RestSmiths, Time - Sleep}),

    {noreply, State};
handle_cast(_, State) ->
    {noreply, State}.

beat(Smiths, 0) ->
    Smiths;
beat([{_, Pid, _, _}|L], ToKill) ->
    exit(Pid, kill),
    beat(L, ToKill - 1).

handle_info(_, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

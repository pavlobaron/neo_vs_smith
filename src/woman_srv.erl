-module(woman_srv).

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

start_link(MatrixPid) ->
    gen_server:start_link({local, woman}, ?MODULE, [MatrixPid], []).

init([MatrixPid]) ->
    erlang:display("Woman: ready"),
    {ok, [MatrixPid]}.

handle_call(eval, _From, State) ->
    {reply, State}.

think(Msg) ->
    erlang:display("Woman thinks: " ++ Msg).

handle_cast({appear, _, _}, State) ->
    erlang:display("Woman with food bags appears"),
    think("What the..?"),

    {noreply, State};
handle_cast({drop, bags, _}, State) ->
    erlang:display("Woman drops her bags on the ground, scared like hell"),
    think("Oops, I did it again"),

    {noreply, State};
handle_cast(_, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

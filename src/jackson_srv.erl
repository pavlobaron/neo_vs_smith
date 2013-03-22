-module(jackson_srv).

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

start_link(MatrixPid) ->
    gen_server:start_link({local, jackson}, ?MODULE, [MatrixPid], []).

init([MatrixPid]) ->
    erlang:display("Jackson: ready"),
    {ok, [MatrixPid]}.

handle_call(eval, _From, State) ->
    {reply, State}.

think(Msg) ->
    erlang:display("Jackson thinks: " ++ Msg).

handle_cast({assimilate, woman, _}, State) ->
    erlang:display("Jackson assimilates Woman, terribly deforming her face"),
    think("begin Assimilate(woman) end."),
    {noreply, State};
handle_cast({say, Msg, _}, State) ->
    erlang:display("Jackson says: " ++ Msg),
    think("begin WriteLn('" ++ Msg ++ "') end."),
    {noreply, State};
handle_cast(_, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

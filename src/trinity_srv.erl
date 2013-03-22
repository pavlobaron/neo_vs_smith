-module(trinity_srv).

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

start_link(MatrixPid) ->
    gen_server:start_link({local, trinity}, ?MODULE, [MatrixPid], []).

init([MatrixPid]) ->
    erlang:display("Trinity: ready"),
    {ok, [MatrixPid]}.

handle_call(eval, _From, State) ->
    {reply, State}.

think(Msg) ->
    erlang:display("Trinity thinks: " ++ Msg).

handle_cast({say, Msg, _}, State) ->
    erlang:display("Trinity says: " ++ Msg),
    think("God, I hope I haven't been too naughty last night. We're even not yet married"),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

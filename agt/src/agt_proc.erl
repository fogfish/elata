%%
%%   Copyright (c) 2011, Nokia Corporation
%%   All rights reserved.
%%
%%    Redistribution and use in source and binary forms, with or without
%%    modification, are permitted provided that the following conditions
%%    are met:
%% 
%%     * Redistributions of source code must retain the above copyright
%%     notice, this list of conditions and the following disclaimer.
%%     * Redistributions in binary form must reproduce the above copyright
%%     notice, this list of conditions and the following disclaimer in
%%     the documentation and/or other materials provided with the
%%     distribution.
%%     * Neither the name of Nokia nor the names of its contributors
%%     may be used to endorse or promote products derived from this
%%     software without specific prior written permission.
%% 
%%   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%%   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%%   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
%%   FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
%%   COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
%%   INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
%%   (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%%   SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
%%   HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
%%   CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
%%   OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
%%   EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE
%%
-module(agt_proc).
-behaviour(gen_server).
-author(dmitry.kolesnikov@nokia.com).

%%
%% ELATA: Agent Process
%%        It is periodcally executed sample and hold process
%%
%% id        - unique job identity SHA1( script )
%% thinktime - defines number of seconds between job runs
%% ttl       - time to live, number of seconds to keep sampling running
%% proc      - process code (script) to be executed
%%
%% Results of job execution are agregated into Process object
%%
%% timestamp - unix timestamp of last sample (local time of process)
%% cycle     - number of executed cycles
%%
%% Additionally
%%   bckt_raw is populated with raw/measured data streams
%%   bckt_doc  - output of script execution
%%

-export([
   start_link/3,
   %% gen_server
   init/1,
   handle_call/3,
   handle_cast/2,
   handle_info/2,
   terminate/2,
   code_change/3
]).

%% Job internall state
-record(srv, {
   bucket,      %% bucket descriptor
   key,         %% unique job identifier
   proc         %% job description                  
   
   %% timestamp,   %% unix timestamp when job is started (in seconds)  
   %% ttl,         %% time-to-live (effective time-to-live)
   %% cycles       %% nbr of executed cycles
}).

%%
%% debug macro
-ifdef(DEBUG).
-define(DEBUG(M), error_logger:info_report(M)).
-else.
-define(DEBUG(M), true).
-endif.


%%
%% 
start_link(Bucket, Key, Proc) ->
   gen_server:start_link(?MODULE, [Bucket, Key, Proc], []).
   
%%
%% Init
init([Bucket, Key, Proc]) ->
   % register itself to keyspace
   Name  = proplists:get_value(name, Bucket),
   ok    = kvs:put({keyspace, Name}, Key, self()),
   think_timer(Proc),
   ?DEBUG([{process, started} | Proc]),
   % return a state
   {ok,
      #srv{
         bucket=Bucket,
         key=Key,
         proc=[{timestamp, timestamp()} | Proc]
       }
    }.
        
%%% set
handle_call({kvs_put, Key, Proc}, _From, State) ->
   {reply, ok, State#srv{proc = [{timestamp, timestamp()} | Proc]} };
   
handle_call({kvs_get, Key}, _From, State) ->
   {reply, {ok, State#srv.proc}, State};
handle_call(_Req, _From, State) ->
   {reply, undefined, State}.
handle_cast({kvs_remove, Key}, State) ->
   {stop, normal, State};
handle_cast(_Req, State) ->
   {noreply, State}.

handle_info(run, State) ->
   Now  = timestamp(),
   Dead = proplists:get_value(timestamp, State#srv.proc) + 
          proplists:get_value(ttl, State#srv.proc, Now),
   if 
      Now < Dead  ->
         % measure performance and record results
         case clib_perf:net(proplists:get_value(code, State#srv.proc)) of
            {ok, [_, Ds]}      ->
               lists:foreach(
                  fun({Tag, Value}) ->
                     ok = kvs:put(bckt_ds, {State#srv.key, Tag}, {Now, Value})
                  end,
                  Ds
               ); 
            {ok, [_, Ds, Doc]} ->
               ok = kvs:put(bckt_doc, State#srv.key, Doc),
               lists:foreach(
                  fun({Tag, Value}) ->
                     ok = kvs:put(bckt_ds, {State#srv.key, Tag}, {Now, Value})
                  end,
                  Ds
               )
         end,
         think_timer(State#srv.proc),
         Cycles = proplists:get_value(cycles, State#srv.proc, 0),
         NProc  = [{cycles, Cycles + 1} | proplists:delete(cycles, State#srv.proc)],
         {noreply, State#srv{proc = NProc}};
      true        ->
         {stop, normal, State}
   end;
handle_info(_Msg, State) ->
   {noreply, State}.
   
terminate(_Reason, State) ->
   ?DEBUG([{process, terminated} | State#srv.proc]),
   % unregister itself from keyspace
   Name = proplists:get_value(name, State#srv.bucket),
   kvs:remove({keyspace, Name}, State#srv.key).
   
code_change(_OldVsn, State, _Extra) ->
   {ok, State}.     

%%%------------------------------------------------------------------   
%%%
%%% Private functions
%%%
%%%------------------------------------------------------------------

% return unix timestamp
timestamp() ->
   {Mega, Sec, _Micro} = erlang:now(),
   Mega * 1000000 + Sec.

% starts think timer   
think_timer(Proc) ->   
   Thinktime = proplists:get_value(thinktime, Proc, 300),
   timer:send_after(Thinktime * 1000, run).
      
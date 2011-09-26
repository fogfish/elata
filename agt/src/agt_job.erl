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
-module(agt_job).
-behaviour(gen_server).
-author(dmitry.kolesnikov@nokia.com).

%%
%% ELATA Job is periodcally executed sample and hold process
%%
%% id        - unique job identity SHA1( script )
%% thinktime - defines number of seconds between job runs
%% ttl       - time to live, number of seconds to keep sampling running
%% script    - script to be executed by job
%%
%% Results of job execution are agregated into telemetry storage
%%
%% id        - unique job identity SHA1( script )
%% timestamp - unix timestamp of last sample (local time of process)
%% cycle     - number of executed samples
%% output    - output of script execution
%% ds        - raw data streams
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
   job,         %% job description                  
   
   timestamp,   %% unix timestamp of last job activity (in seconds)  
   ttl,         %% time-to-live (effective time-to-live)
   cycles       %% nbr of executed cycles
}).

%%
%% 
start_link(Bucket, Key, Job) ->
   gen_server:start_link(?MODULE, [Bucket, Key, Job], []).
   
%%
%% Init
init([Bucket, Key, Job]) ->
   % register itself to keyspace
   Name  = proplists:get_value(name, Bucket),
   ok    = kvs:put({keyspace, Name}, Key, self()),
   think_timer(Job),
   error_logger:info_report([{job, started} | Job]),
   % return a state
   {ok,
      #srv{
         bucket=Bucket,
         key=Key,
         job=Job,
         timestamp=timestamp(),
         ttl=proplists:get_value(ttl, Job),
         cycles=0
       }
    }.
        
%%% set
handle_call({kvs_put, Key, Job}, _From, #srv{bucket=Bucket} = State) ->
   % reset a job
   NState = #srv{
      bucket=Bucket,
      key=Key,
      job=Job,
      timestamp=timestamp(),
      ttl=proplists:get_value(ttl, Job),
      cycles=0
   },
   {reply, ok, NState};
   
handle_call({kvs_get, Key}, _From, #srv{job = Job} = State) ->
   {reply, {ok, Job}, State};
handle_call(_Req, _From, State) ->
   {reply, undefined, State}.
handle_cast({kvs_remove, Key}, State) ->
   {stop, normal, State};
handle_cast(_Req, State) ->
   {noreply, State}.

handle_info(sample, #srv{key=Key, job=Job, timestamp=Time, ttl=undefined, cycles=Cycles} = State) ->
   % handles permanent jobs
   execute_job(Key, Job),
   think_timer(Job),
   {noreply, State#srv{timestamp=timestamp(), cycles=Cycles + 1}};
handle_info(sample, #srv{key=Key, job=Job, timestamp=Time, ttl=TTL, cycles=Cycles} = State) ->
   % handles temporary jobs
   Tlag = timestamp() - Time,
   Nttl = TTL - Tlag,
   if
      Nttl > 0 ->
         execute_job(Key, Job),
         think_timer(Job),
         {noreply, State#srv{timestamp=timestamp(), cycles=Cycles + 1, ttl=Nttl}};
      true     ->
         {stop, normal, State}
   end;   
handle_info(_Msg, State) ->
   {noreply, State}.
   
terminate(_Reason, #srv{bucket = Bucket, key = Key, job = Job} = State) ->
   % unregister itself from keyspace
   Name = proplists:get_value(name, Bucket),
   ok   = kvs:remove({keyspace, Name}, Key),
   error_logger:info_report([{job, terminated} | Job]),
   ok.
   
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
think_timer(Job) ->   
   Thinktime = proplists:get_value(thinktime, Job, 300),
   timer:send_after(Thinktime * 1000, sample).
   
% executes a job   
execute_job(Key, Job) ->
   {ok, DS, Data} = job_script:execute(proplists:get_value(script, Job)),
   Time = timestamp(),
   %error_logger:info_report(lists:append(Job, DS)),
   kvs:put(elata_telemetry, Key, lists:append([{timestamp, Time}], DS)),
   kvs:put(elata_document,  Key, [{timestamp, Time}, {content, Data}]),
   ok.
   
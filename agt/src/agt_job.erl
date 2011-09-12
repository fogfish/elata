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
%% ELATA Job is periodcally executed Sample and Hold process
%%
%% id        - unique job identity SHA1( script )
%% frequency - defines number of sample per unit of time 
%% ttl       - time to live, number of seconds to keep sampling running
%% script    - script to be executed by job 
%% cycle     - number of executed samples
%% timestamp - unix timestamp of last sample (local time of process)
%% output    - output of script execution
%% sample    - sampled data streams in sample and hold manner (script output)
%%


-export([
   start_link/2,
   %% gen_server
   init/1,
   handle_call/3,
   handle_cast/2,
   handle_info/2,
   terminate/2,
   code_change/3
]).

%%
%% 
start_link(Key, Job) ->
   gen_server:start_link(?MODULE, [Key, Job], []).
   
%%
%% Init
init([Key, Job]) ->
   Freq = proplists:get_value(frequency, Job),
   Idle = erlang:round( 1 / Freq * 1000 ),
   kvs_reg:register(Key, self()),
   timer:send_after(Idle, sample),
   {ok, {Key, Job}}.

handle_call({kvs_set, Item}, _From, {Key, _}) ->
   {reply, ok, {Key, Item}};
handle_call(kvs_get, _From, {Key, Item}) ->
   {reply, {ok, Item}, {Key, Item}};
handle_call(_Req, _From, State) ->
   {reply, undefined, State}.
handle_cast(kvs_destroy, State) ->
   {stop, normal, State};
handle_cast(_Req, State) ->
   {noreply, State}.

handle_info(sample, {Key, Job}) ->
   NJob = sample_job(Job),
   case proplists:get_value(ttl, NJob, 0) of
      TTL when TTL > 0 ->
         %% re-schedule job
         Freq = proplists:get_value(frequency, NJob),
         Idle = erlang:round( 1 / Freq * 1000 ),
         timer:send_after(Idle, sample),
         {noreply, {Key, NJob}};
      TTL when TTL =< 0 ->
         {stop, normal, {Key, NJob}}   
   end;   
handle_info(_Msg, State) ->
   {noreply, State}.
   
terminate(_Reason, {Key, _}) ->
   kvs_reg:unregister(Key),
   ok.
   
code_change(_OldVsn, State, _Extra) ->
   {ok, State}.     

%%%------------------------------------------------------------------   
%%%
%%% Private functions
%%%
%%%------------------------------------------------------------------
sample_job(Job) ->
   % refresh job timestamps
   Job1 = job_timestamp(Job),
   % increment number of cycles
   job_cycles(Job1).
   % run a job script
   
% calculates a job timestamp   
job_timestamp(Job) ->
   {MegaSecs, Secs, _MicroSecs} = erlang:now(),
   TS = MegaSecs * 1000000 + Secs,
   case proplists:get_value(timestamp, Job) of
      undefined -> 
         [{timestamp, TS} | Job];
      Timestamp ->
         [{timestamp, TS} | job_ttl(proplists:delete(timestamp, Job), TS - Timestamp)]
   end.
   
% updates job time-to-live
job_ttl(Job, Delta) ->
   case proplists:get_value(ttl, Job) of
      undefined -> 
         Job;
      TTL       ->
         [{ttl, TTL - Delta} | proplists:delete(ttl, Job)]
   end.
   
% increment job cycles
job_cycles(Job) ->
   Cycle = proplists:get_value(cycle, Job, 0),
   [{cycle, Cycle + 1} | proplists:delete(cycle, Job)].
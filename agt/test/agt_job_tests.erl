%%
%%   Copyright (c) 2011, Nokia Corporation
%%   All Rights Reserved.
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
-module(agt_job_tests).
-author(dmitry.kolesnikov@nokia.com).
-include_lib("eunit/include/eunit.hrl").

%%
%% Agent Job test interface
%%

agt_job_test_() ->
   {
      setup,
      fun setup/0,
      [
      { "Create job", fun create_job/0},
      { "Lookup job", fun lookup_job/0},
      { "Insert job", fun insert_job/0},
      { "Delete job", fun delete_job/0},
      { "Job life-cycle", fun job_life_cycle/0}
      ]
   }.
   
%%% test job   
-define(JOB, [
   {id,        test_job_1},
   {frequency, 1},
   {ttl,       3},
   {script,    undefined}
]).   
   
setup() ->
   kvs_reg:start(),
   kvs_cache_sup:start_link(),
   agt_job_sup:start_link(),
   keyval_bucket:create(test_job, [{plugin, agt_job_sup}, {key, id}]).
   
%%%
%%% Job CRUD
%%%
create_job() ->
   ?assert(
      ok =:= keyval_store:create(test_job, ?JOB)
   ).
   
lookup_job() ->
   ?assert(
      {ok, ?JOB} =:= keyval_store:lookup(test_job, test_job_1)
   ).

insert_job() ->
   NewJob = [{ttl, 60} | proplists:delete(ttl, ?JOB)],
   ?assert(
      ok =:= keyval_store:insert(test_job, NewJob)
   ),
   ?assert(
      {ok, NewJob} =:= keyval_store:lookup(test_job, test_job_1)
   ).
   
delete_job() ->
   ?assert(
      ok =:= keyval_store:delete(test_job, test_job_1)
   ),
   ?assert(
      {error, not_found} =:= keyval_store:lookup(test_job, test_job_1)
   ).

%%%
%%% Job Sample
%%%
job_life_cycle() ->
   ?assert(
      ok =:= keyval_store:create(test_job, ?JOB)
   ),
   timer:sleep(1000),
   % 1st measurement is performed cycles increased
   {ok, Job1} = keyval_store:lookup(test_job, test_job_1),
   
   ?assert(
      0 < proplists:get_value(cycle, Job1)
   ),
   timer:sleep(1000),
   % 2nd measurment is performed, ttl is decreased
   {ok, Job2} = keyval_store:lookup(test_job, test_job_1),
   ?assert(
      3 > proplists:get_value(ttl, Job2)
   ),
   timer:sleep(2000),
   % ttl is expired
   ?assert(
      {error, not_found} =:= keyval_store:lookup(test_job, test_job_1)
   ).

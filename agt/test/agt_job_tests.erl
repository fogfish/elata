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
      { "Put permanent job", fun pjob_put/0},
      { "Get permanent job", fun pjob_get/0},
      { "Remove permanent job", fun pjob_remove/0},
      { "Job life-cycle 1", {timeout, 5, fun job_lc1/0}},
      { "Job life-cycle 2", {timeout, 10, fun job_lc2/0}}
      ]
   }.
   
%%% test job   
-define(PJOB, [
   {id,        test_job_1},
   {thinktime, 1},
   {ttl,       undefined},
   {script,    undefined}
]).   
   
-define(TJOB, [
   {id,        test_job_2},
   {thinktime, 1},
   {ttl,       3},
   {script,    undefined}
]).

setup() ->
   ok = application:start(elata_kvs),
   kvs_bucket:define(test_job, [{storage, agt_job_sup}]).
   
%%%
%%% Job CRUD
%%%
pjob_put() ->
   ?assert(
      ok =:= kvs:put(test_job, test_job_1, ?PJOB)
   ).
   
pjob_get() ->
   ?assert(
      {ok, ?PJOB} =:= kvs:get(test_job, test_job_1)
   ).

pjob_remove() ->
   ?assert(
      ok =:= kvs:remove(test_job, test_job_1)
   ),
   timer:sleep(100),
   ?assert(
      false =:= kvs:has(test_job, test_job_1)
   ).
   
job_lc1() ->
   ?assert(
      ok  =:= kvs:put(test_job, test_job_2, ?TJOB)
   ),
   timer:sleep(1000),
   ?assert(
      {ok, ?TJOB} =:= kvs:get(test_job, test_job_2)
   ),
   timer:sleep(1000),
   ?assert(
      {ok, ?TJOB} =:= kvs:get(test_job, test_job_2)
   ),
   timer:sleep(1000),
   ?assert(
      {error, not_found} =:= kvs:get(test_job, test_job_2)
   ).

job_lc2() ->
   ?assert(
      ok =:= kvs:put(test_job, test_job_2, ?TJOB)
   ),
   timer:sleep(1000),
   ?assert(
      {ok, ?TJOB} =:= kvs:get(test_job, test_job_2)
   ),
   timer:sleep(1000),
   ?assert(
      ok =:= kvs:put(test_job, test_job_2, ?TJOB)
   ),
   timer:sleep(1000),
   ?assert(
      {ok, ?TJOB} =:= kvs:get(test_job, test_job_2)
   ),
   timer:sleep(1000),
   ?assert(
      {ok, ?TJOB} =:= kvs:get(test_job, test_job_2)
   ),
   timer:sleep(1000),
   ?assert(
      {error, not_found} =:= kvs:get(test_job, test_job_2)
   ).   

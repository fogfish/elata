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
-module(kvs_evt_log_tests).
-author(dmitry.kolesnikov@nokia.com).
-include_lib("eunit/include/eunit.hrl").


kvs_bin_log_test_() ->
   {
      setup,
      fun setup/0,
      [
      { "Put item", fun put/0},
      { "Remove item", fun remove/0},
      { "Expire", {timeout, 10, fun expire/0}}
      ]
   }.
   
setup() ->
   kvs_sup:start_link(),
   kvs_evt_sup:subscribe({kvs_evt_log, [[{ttl, 5}, {size, 10}]]}),
   kvs_bucket:define(kvs_evt_log, [{storage, kvs_cache_sup}]),
   kvs_bucket:define(test, [{storage, kvs_sys}, event, evtlog]).



put()  ->
   Key = {key, 1},
   ok = kvs:put(test, Key, {val, 1}),
   timer:sleep(100),
   ?assert(
      {ok, [{ttl, 5}, {0, put, test, Key}]} =:= kvs:get(kvs_evt_log, 0)
   ).
   
remove() ->
   Key = {key, 1},
   ok = kvs:remove(test, Key),
   timer:sleep(100),
   ?assert(
      {ok, [{ttl, 5}, {0, put, test, Key}, {1, remove, test, Key}]} =:= kvs:get(kvs_evt_log, 0)
   ).
   
expire() ->   
   timer:sleep(5500),
   ?assert(
      {error, not_found} =:= kvs:get(kvs_evt_log, 0)
   ).
   
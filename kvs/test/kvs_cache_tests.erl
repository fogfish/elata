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
-module(kvs_cache_tests).
-author(dmitry.kolesnikov@nokia.com).
-include_lib("eunit/include/eunit.hrl").

%%
%% Unit test of KVS proxy to ETS via keyval_store interface 
%%

kvs_cache_test_() ->
   {
      setup,
      fun setup/0,
      [                                      
      { "Put item", fun put/0},
      { "Has item", fun has/0},
      { "Get item", fun get/0},
      { "Has item", fun remove/0}
      ]
   }.

setup() ->
   kvs_sup:start_link(),
   kvs_bucket:define(test, [{storage, kvs_cache_sup}]).
   
%%%
%%% 
%%%
put() ->
   ?assert(
      ok =:= kvs:put(test, a, {a, b, c})
   ).
   
has() ->
   ?assert(
      true  =:= kvs:has(test, a)
   ),
   ?assert(
      false =:= kvs:has(test, b)
   ).
   
get() ->
   ?assert(
      {ok, {a, b, c}} =:= kvs:get(test, a)
   ),
   ?assert(
      {error, not_found} =:= kvs:get(test, b)
   ).
   
remove() ->
   ?assert(
      ok =:= kvs:remove(test, a)
   ),
   timer:sleep(100),
   ?assert(
      {error, not_found} =:= kvs:get(test, a)
   ).


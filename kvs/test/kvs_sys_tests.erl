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
-module(kvs_sys_tests).
-author(dmitry.kolesnikov@nokia.com).
-include_lib("eunit/include/eunit.hrl").

kvs_sys_test_() ->
   {
      setup,
      fun() ->
         {ok, _} = kvs_sys:construct([kvs_sys_ref]),
         {ok, _} = kvs_sys:construct([kvs_sys_bucket])
      end,
      fun(X) ->
         ets:delete(kvs_sys_ref),
         ets:delete(kvs_sys_bucket)
      end,
      [
         {"Put item into sys bucket", fun put/0},
         {"Has item in sys bucket", fun has/0},
         {"Get item in sys bucket", fun get/0},
         {"Remove item in sys bucket", fun remove/0}
      ]
   }.

kvs_sys_api_test_() ->
   {
      setup,
      fun() ->
         {ok, _} = kvs_sys:construct([kvs_sys_ref]),
         {ok, _} = kvs_sys:construct([kvs_sys_bucket])
      end,
      fun(X) ->
         ets:delete(kvs_sys_ref),
         ets:delete(kvs_sys_bucket)
      end,
      [
         {"KVS Put", fun kvs_put/0},
         {"KVS Has", fun kvs_has/0},
         {"KVS Get", fun kvs_get/0},
         {"KVS Remove", fun kvs_remove/0}
      ]
  }.
  
put() ->
   ?assert(
      ok =:= kvs_sys:put(kvs_sys_bucket, test, {a, b, c})
   ).

has() ->
   ?assert(
      true =:= kvs_sys:has(kvs_sys_bucket, test)
   ).
   
get() ->
   ?assert(
      {ok, {a, b, c}} =:= kvs_sys:get(kvs_sys_bucket, test)
   ).
   
remove() ->
   ?assert(
      ok =:= kvs_sys:remove(kvs_sys_bucket, test)
   ),
   ?assert(
      {error, not_found} =:= kvs_sys:get(kvs_sys_bucket, test)
   ).

kvs_put() ->
   ?assert(
      {ok, test} =:= kvs:put(kvs_sys_bucket, [{name, test}, {val, abc}])
   ).
   
kvs_has() ->
   ?assert(
      true  =:= kvs:has(kvs_sys_bucket, test)
   ),
   ?assert(
      false =:= kvs:has(kvs_sys_bucket, undef)
   ).

kvs_get() ->
   ?assert(
      {ok, [{name, test}, {val, abc}]} =:= kvs:get(kvs_sys_bucket, test)
   ).
   
kvs_remove() ->
   ?assert(
      ok =:= kvs:remove(kvs_sys_bucket, test)
   ),
   ?assert(
      {error, not_found} =:= kvs:get(kvs_sys_bucket, test)
   ).
   

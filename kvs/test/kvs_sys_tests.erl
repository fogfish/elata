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
         {ok, _} = kvs_sys:start_link(kvs_sys_ref),
         {ok, _} = kvs_sys:start_link(kvs_sys_cat)
      end,
      fun(X) ->
         ets:delete(kvs_sys_ref),
         ets:delete(kvs_sys_cat)
      end,
      [
         {"New sys", fun new/0},
         {"Put sys", fun put/0},
         {"Get sys", fun get/0},
         {"Remove sys", fun remove/0}
      ]
   }.
  
new() ->
   {ok, _} = kvs:new(test, [{storage, kvs_sys}]),
   {ok, _} = kvs:get(kvs_sys_ref, test),
   {ok, _} = kvs:get(kvs_sys_cat, test).
   
put() ->
   ?assert(
      ok =:= kvs:put(test, key, self())
   ).

get() ->
   ?assert(
      {ok, self()} =:= kvs:get(test, key)
   ).
   
remove() ->
   ?assert(
      ok =:= kvs:remove(test, key)
   ),
   ?assert(
      {error, not_found} =:= kvs:get(test, key)
   ).



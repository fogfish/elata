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
-module(kvs_dets_tests).
-author(dmitry.kolesnikov@nokia.com).
-include_lib("eunit/include/eunit.hrl").

%%
%% Unit test of KVS proxy to ETS via keyval_store interface 
%%

kvs_dets_test_() ->
   {
      setup,
      fun() ->
         kvs:start(),
         {ok, _} = kvs:new("kvs:test", [{storage, kvs_dets}, {file, "/tmp/test.dets"}]),
         true = filelib:is_file("/tmp/test.dets")
      end,
      fun(_) ->
         file:delete("/tmp/test.dets")
      end,
      [                                      
      { "Put item", fun put/0},
      { "Has item", fun has/0},
      { "Get item", fun get/0},
      { "Remove item", fun remove/0},
      { "Map items", fun map/0},
      { "Fold items", fun fold/0}
      ]
   }.
   
-define(VAL, "value").   
   
%%%
%%% 
%%%
put() ->
   R = kvs:put("kvs:test", key, ?VAL),
   ?assert(
      ok =:= R
   ).
   
has() ->
   ?assert(
      true  =:= kvs:has("kvs:test", key)
   ),
   ?assert(
      false =:= kvs:has("kvs:test", nokey)
   ).
   
get() ->
   ?assert(
      {ok, ?VAL} =:= kvs:get("kvs:test", key)
   ),
   ?assert(
      {error, not_found} =:= kvs:get("kvs:test", nokey)
   ).
   
remove() ->
   ?assert(
      ok =:= kvs:remove("kvs:test", key)
   ),
   timer:sleep(100),
   ?assert(
      {error, not_found} =:= kvs:get("kvs:test", key)
   ).

map() ->
   lists:foreach(
      fun(X) -> kvs:put("kvs:test", X, X) end,
      lists:seq(1, 5)
   ),
   R1 = kvs:map("kvs:test", fun(_, V) -> V * V end),
   lists:foreach(
      fun(X) -> kvs:remove("kvs:test", X) end,
      lists:seq(1, 5)
   ),
   ?assert( [1, 4, 9, 16, 25] =:= R1).
   
fold() ->
   lists:foreach(
      fun(X) -> kvs:put("kvs:test", X, X) end,
      lists:seq(1, 5)
   ),
   R1 = kvs:fold("kvs:test", 0, fun(_, V, Acc) -> Acc + V end),
   lists:foreach(
      fun(X) -> kvs:remove("kvs:test", X) end,
      lists:seq(1, 5)
   ),
   ?assert( R1 =:= 15 ).

   

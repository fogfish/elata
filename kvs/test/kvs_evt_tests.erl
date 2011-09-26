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
-module(kvs_evt_tests).
-author(dmitry.kolesnikov@nokia.com).
-include_lib("eunit/include/eunit.hrl").

-behaviour(gen_event).
-export([
   init/1,
   handle_event/2,
   handle_call/2,
   handle_info/2,
   terminate/2,
   code_change/3
]).


%%
%% Unit test of KVS event
%%
kvs_evt_test_() ->
   {
      setup,
      fun setup/0,
      [
      { "Put    event", fun put/0},
      { "Remove event", fun remove/0}
      ]
   }.
   
setup() ->
   kvs_sup:start_link(),
   kvs_evt_sup:subscribe(kvs_evt_tests),
   kvs_bucket:define(test_src, [event, {storage, kvs_cache_sup}]),
   kvs_bucket:define(test_dst, [{storage, kvs_sys}]).
   

put() ->   
   ?assert(
      ok =:= kvs:put(test_src, a, {a, b, c})
   ),
   timer:sleep(100),
   ?assert(
      {ok, {a, b, c}} =:= kvs:get(test_dst, a)
   ).   
   
remove() ->
   ?assert(
      ok =:= kvs:remove(test_src, a)
   ),
   timer:sleep(100),
   ?assert(
      {error, not_found} =:= kvs:get(test_dst, a)
   ).

   
%%%------------------------------------------------------------------
%%%
%%%  gen_event
%%%
%%%------------------------------------------------------------------
init([]) -> 
   {ok, []}.
   
handle_event({put, Bucket, Key, Item}, State) ->
   ok = kvs:put(test_dst, Key, Item),
   {ok, State};
handle_event({remove, Bucket, Key, Item}, State) ->
   kvs:remove(test_dst, Key),
   {ok, State};
handle_event(Evt, State) ->
   {ok, State}.
   
handle_call(_Req, State) ->
   {ok, undefined, State}.
   
handle_info(_Msg, State) ->
   {ok, State}.
   
terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) -> 
   {ok, State}.
   
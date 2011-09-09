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
      { "Create event", fun create/0},
      { "Insert event", fun insert/0},
      { "Delete event", fun delete/0}
      ]
   }.

-define(ENTITY_1, [{id, entity_1}, {name, "Test Entity 1"}]).
-define(ENTITY_2, [{id, entity_1}, {name, "Test Entity 2"}]).   
   
setup() ->
   % start registry
   kvs_reg:start(),
   kvs_cache_sup:start_link(),
   % start event
   kvs_evt:start_link(),
   kvs_evt_sup:start_link(),
   kvs_evt_sup:factory(kvs_evt_tests),
   % start storage
   kvs_cache_sup:start_link(),
   kvs_ets_sup:start_link(),
   keyval_bucket:create(test_src, [event, {plugin, kvs_cache_sup}, {key, id}]),
   keyval_bucket:create(test_dst, [singleton, {plugin, kvs_ets_sup}, {key, id}]).
   

create() ->   
   ?assert(
      ok =:= keyval_store:create(test_src, ?ENTITY_1)
   ),
   timer:sleep(100),
   ?assert(
      {ok, ?ENTITY_1} =:= keyval_store:lookup(test_dst, entity_1)
   ).   

insert() ->   
   ?assert(
      ok =:= keyval_store:insert(test_src, ?ENTITY_1)
   ),
   timer:sleep(100),
   ?assert(
      {ok, ?ENTITY_1} =:= keyval_store:lookup(test_dst, entity_1)
   ).
   
delete() ->
   ?assert(
      ok =:= keyval_store:delete(test_src, entity_1)
   ),
   timer:sleep(100),
   ?assert(
      {error, not_found} =:= keyval_store:lookup(test_dst, entity_1)
   ).

   
%%%------------------------------------------------------------------
%%%
%%%  gen_event
%%%
%%%------------------------------------------------------------------
init([]) -> 
   {ok, []}.
   
handle_event({create, Ns, Entity}, State) ->
   keyval_store:create(test_dst, Entity),
   {ok, State};
handle_event({insert, Ns, Entity}, State) ->
   keyval_store:insert(test_dst, Entity),
   {ok, State};
handle_event({delete, Ns, Key}, State) ->
   keyval_store:delete(test_dst, Key),
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
   
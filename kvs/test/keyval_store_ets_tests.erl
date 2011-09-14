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
-module(keyval_store_ets_tests).
-author(dmitry.kolesnikov@nokia.com).
-include_lib("eunit/include/eunit.hrl").

%%
%% Unit test of KVS proxy to ETS via keyval_store interface 
%%

keyval_store_ets_test_() ->
   {
      setup,
      fun setup/0,
      [
      { "Create entity as list", fun create_as_list/0},
      { "Lookup entity as list", fun lookup_as_list/0},
      { "Insert entity as list", fun insert_as_list/0},
      { "Delete entity as list", fun delete_as_list/0},
      
      { "Create entity as tuple", fun create_as_tuple/0},
      { "Lookup entity as tuple", fun lookup_as_tuple/0},
      { "Insert entity as tuple", fun insert_as_tuple/0},
      { "Delete entity as tuple", fun delete_as_tuple/0}
      ]
   }.

-define(LST_DOMAIN,   [singleton, {plugin, kvs_ets_sup}, {key, id}]).
-define(REC_DOMAIN,   [singleton, {plugin, kvs_ets_sup}, {key, 2}]).

-define(LST_ENTITY_1, [{id, entity_1}, {name, "Test Entity 1"}]).
-define(LST_ENTITY_2, [{id, entity_1}, {name, "Test Entity 2"}]).
-define(REC_ENTITY_1, {rec, entity_1, "Test Entity 1"}).
-define(REC_ENTITY_2, {rec, entity_1, "Test Entity 2"}).


setup() ->
   kvs_sup:start_link(),
   keyval_bucket:create(test_lst, ?LST_DOMAIN),
   keyval_bucket:create(test_rec, ?REC_DOMAIN).
   
%%%
%%% Entity is List of key-value pairs
%%%
create_as_list() ->
   ?assert(
      ok =:= keyval_store:create(test_lst, ?LST_ENTITY_1)
   ).
   
lookup_as_list() ->
   ?assert(
      {ok, ?LST_ENTITY_1} =:= keyval_store:lookup(test_lst, entity_1)
   ).

insert_as_list() ->
   ?assert(
      ok =:= keyval_store:insert(test_lst, ?LST_ENTITY_2)
   ),
   ?assert(
      {ok, ?LST_ENTITY_2} =:= keyval_store:lookup(test_lst, entity_1)
   ).
   
delete_as_list() ->
   ?assert(
      ok =:= keyval_store:delete(test_lst, entity_1)
   ),
   ?assert(
      {error, not_found} =:= keyval_store:lookup(test_lst, entity_1)
   ).

%%%
%%% Entity is tuple
%%%
create_as_tuple() ->
   ?assert(
      ok =:= keyval_store:create(test_rec, ?REC_ENTITY_1)
   ).
   
lookup_as_tuple() ->
   ?assert(
      {ok, ?REC_ENTITY_1} =:= keyval_store:lookup(test_rec, entity_1)
   ).   
   
insert_as_tuple() ->
   ?assert(
      ok =:= keyval_store:insert(test_rec, ?REC_ENTITY_2)
   ),
   ?assert(
      {ok, ?REC_ENTITY_2} =:= keyval_store:lookup(test_rec, entity_1)
   ).
   
delete_as_tuple() ->
   ?assert(
      ok =:= keyval_store:delete(test_rec, entity_1)
   ),
   ?assert(
      {error, not_found} =:= keyval_store:lookup(test_rec, entity_1)
   ).   
   
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
-module(keyval_store).
-author(dmitry.kolesnikov@nokia.com).

%%
%% Key/Value Store provides CRUD interface to manage key/val items   
%% Interface operates with 'item' (E) that is set of attributes.
%%
%% E {(k,v): k - term(), v - any()}
%%
%% Run-time entity representation implies either list of 
%% attribute-value pairs or run-time tuples/record. The storage domain cannot 
%% mix attribute lists or tuples, one of them should be chosen.
%%

-export([
   create/2,
   insert/2,
   lookup/2,
   lookup/3,
   delete/2
]).

%%
%% TODO guarg condition
%% is_tuple
%% is_list on CRUD operations


%%
%% create new item
create(Bucket, Item) ->
   case keyval_bucket:lookup(Bucket) of
      {error, _}  ->
         % bucket is not declared
         {error, undefined_bucket};
      {ok, B} ->
         % bucket is found, its config is returned
         Result = case proplists:is_defined(singleton, B) of
            % domain is managed by single process
            true  -> gen_kvs_singleton:do_create(B, Item);
            % process per entity mode
            false -> gen_kvs_entity:do_create(B, Item)
         end,
         % TODO: bucket vclock update
         notify(create, Result, Bucket, Item, B),
         Result
   end.   

   
%%
%% insert/update item
insert(Bucket, Item) ->
   case keyval_bucket:lookup(Bucket) of
      {error, _}  ->
         % bucket is not declared
         {error, undefined_bucket};
      {ok, B} ->
         % bucket is found, its config is returned
         Result = case proplists:is_defined(singleton, B) of
            % bucekt is managed by single process
            true  -> gen_kvs_singleton:do_insert(B, Item);
            % process per entity mode
            false -> gen_kvs_entity:do_insert(B, Item)
         end,
         % TODO: bucket vclock update
         notify(insert, Result, Bucket, Item, B),
         Result
   end.


%%
%% read value
lookup(Bucket, Key) ->
   case keyval_bucket:lookup(Bucket) of
      {error, _}  ->
         % bucket is not declared
         {error, undefined_bucket};
      {ok, B} ->
         % bucket is found, its config is returned
         Result = case proplists:is_defined(singleton, B) of
            % bucket is managed by single process
            true  -> gen_kvs_singleton:do_lookup(B, Key);
            % process per entity mode
            false -> gen_kvs_entity:do_lookup(B, Key)
         end,
         notify(lookup, Result, Bucket, Key, B),
         Result
   end.

   
%% 
%% read value with default value
lookup(Bucket, Key, Default) ->
   case keyval_store:lookup(Bucket, Key) of
      {error, _} ->
         {ok, Default};
      Result ->
         Result
   end.
   
%%
%% delete key from bucket       
delete(Bucket, Key) ->
   case keyval_bucket:lookup(Bucket) of
      {error, _}  ->
         % bucket is not declared
         {error, undefined_bucket};
      {ok, B} ->
         % domain is found, its config is returned
         Result = case proplists:is_defined(singleton, B) of
            % domain is managed by single process
            true  -> gen_kvs_singleton:do_delete(B, Key);
            % process per entity mode
            false -> gen_kvs_entity:do_delete(B, Key)
         end,
         % TODO: bucket vclock update
         notify(delete, Result, Bucket, Key, B),
         Result
   end.

   
%%%------------------------------------------------------------------
%%%
%%% Private Functions
%%%
%%%------------------------------------------------------------------
notify(create, ok, Bname, Item, Bucket) ->
   case proplists:is_defined(event, Bucket) of
      true  -> kvs_evt:create(Bname, Item);
      false -> ok
   end;

notify(insert, ok, Bname, Item, Bucket) ->
   case proplists:is_defined(event, Bucket) of
      true  -> kvs_evt:insert(Bname, Item);
      false -> ok
   end;

%%% disabled due to performance implication  
%%%notify(lookup, {ok, _}, Bname, Key, Bucket) ->
%%%   case proplists:is_defined(event, Bucket) of
%%%      true  -> kvs_evt:lookup(Bname, Key);
%%%      false -> ok
%%%   end;   

notify(delete, ok, Bname, Item, Bucket) ->
   case proplists:is_defined(event, Bucket) of
      true  -> kvs_evt:delete(Bname, Item);
      false -> ok
   end;
   
notify(_, _, _, _, _) -> 
   ok.

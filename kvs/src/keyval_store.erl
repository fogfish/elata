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
%% ELATA: Key/Value Store interface/wrapper to storage implementations
%%

%%
%% Public API
-export([
   define/2,
   create/2,
   insert/2,
   lookup/2,
   lookup/3,
   delete/2,
   to_list/1,
   import/2
]).

%%
%% define new bucket
define(Bucket, Opts) ->
   kvs_reg:define(Bucket, Opts),
   kvs_reg:start_bucket(Bucket).

%%
%% create new unique value
create(Bucket, Value) ->
   Result = case kvs_reg:resolve(Bucket) of
      {error, _} ->
         {ok, Pid} = kvs_reg:start_bucket(Bucket),
         kvs_bucket:create(Pid, Value);
      {ok,  Pid} ->
         kvs_bucket:create(Pid, Value)
   end,
   do_notify(create, Result, Bucket, Value),
   Result.
   

%%
%% insert/update value
insert(Bucket, Value) ->
   Result = case kvs_reg:resolve(Bucket) of
      {error, _} ->
         {ok, Pid} = kvs_reg:start_bucket(Bucket),
         kvs_bucket:insert(Pid, Value);
      {ok, Pid} ->
         kvs_bucket:insert(Pid, Value)
   end,
   do_notify(insert, Result, Bucket, Value),
   Result.

%%
%% read value
lookup(Bucket, Key) ->
   Result = case kvs_reg:resolve(Bucket) of
      {error, _} ->
         {error, not_found};
      {ok, Pid} ->
         kvs_bucket:lookup(Pid, Key)
   end,
   do_notify(lookup, Result, Bucket, Key),
   Result.
 
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
   Result = case kvs_reg:resolve(Bucket) of
      {error, _} ->
         ok;
      {ok, Pid} ->
         kvs_bucket:delete(Pid, Key)
   end,
   do_notify(delete, Result, Bucket, Key),
   Result.

%%
%% export to list
to_list(Bucket) ->
   case kvs_reg:resolve(Bucket) of
      {error, _} ->
         {error, undefined};
      {ok,  Pid} ->
         kvs_bucket:to_list(Pid)
   end.
   
%%
%% Import form file
import(Bucket, Filename) ->
   {ok, List} = file:consult(Filename),
   keyval_store:create(Bucket, List).
   
   
%%%------------------------------------------------------------------
%%%
%%% Private Functions
%%%
%%%------------------------------------------------------------------
do_notify(create, ok, Bucket, Value) -> 
   case kvs_reg:is_notify(Bucket) of
      true  -> kvs_evt:create(Bucket, Value);
      false -> ok
   end;

do_notify(insert, ok, Bucket, Value) ->
   case kvs_reg:is_notify(Bucket) of
      true  -> kvs_evt:insert(Bucket, Value);
      false -> ok
   end;

do_notify(lookup, {ok, _}, Bucket, Key) ->
   case kvs_reg:is_notify(Bucket) of
      true  -> kvs_evt:lookup(Bucket, Key);
      false -> ok
   end;

do_notify(delete, ok, Bucket, Key) ->
   case kvs_reg:is_notify(Bucket) of
      true  -> kvs_evt:delete(Bucket, Key);
      false -> ok
   end;

do_notify(_,_,_,_) ->
   ok.
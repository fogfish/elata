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
-module(keyval_bucket).
-author(dmitry.kolesnikov@nokia.com).

%%
%% Key/Value Bucket provides management interface to storage buckets 
%% The interface impacts bucket metadata registered within node
%% Bucket entityes are not impacted
%%

-export([
   create/2,                    
   lookup/1,
   delete/1
]).

%%
%% Create definition of storage domain (domain entities are not impacted)
%%
%% Name   = string() unique domain name
%% Bucket = [opt]
%%    opt = singleton | event | {name, Name} | 
%%          {plugin, Module} | {plugin, {Module, Args}} | 
%%          {key, Id}
%%    singleton = single process manages all domain entities (e.g. proxy to storage)
%%    event = storage domain generates events on CRUD operations
%%    Module = KVS plugin implementation of gen_kvs_domain / gen_kvs_entity
%%    Args = list(), list of arguments supplied to factory method
%%    Id = name of key attribute or key position
create(Name, Bucket) ->
   % TODO: assert: plugin + key
   % TODO: assert & inject dual-time vclock
   Bkey = {kvs_sys_bucket, Name},
   case kvs_reg:resolve(Bkey) of
      {error, _} ->
         % define a plugin factory as part of root supervior tree
         ok = kvs_sup:attach(proplists:get_value(plugin, Bucket)),
         {ok, _Pid} = kvs_cache_sup:construct([Bkey, [{name, Name} | Bucket]]),
         error_logger:info_report([{name, Name} | Bucket]),
         ok;
      {ok,  _Pid} -> 
         {error, already_exists}
   end.

%%
%%
lookup(Name) ->
   Bkey = {kvs_sys_bucket, Name},
   case kvs_reg:resolve(Bkey) of
      {ok, Pid} -> kvs_cache_sup:get(Pid);
      Error     -> Error
   end.

%%
%% Delete definition of storage domain (domain entities are not impacted)
delete(Name) ->
   Bkey = {kvs_sys_bucket, Name},
   case kvs_reg:resolve(Bkey) of
      {ok, Pid} -> kvs_cache_sup:destroy(Pid);
      Error     -> Error
   end.

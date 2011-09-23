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
-module(kvs_sys).
-author(dmitry.kolesnikov@nokia.com).

%%
%% kvs_sys is a system buckets, used by kvs application to manage buckets metadata
%%

-export([
   % public API
   construct/1,
   config/0,
   put/3,
   has/2,
   get/2,
   remove/2
]).

%%
%%
construct([kvs_sys_ref]) ->
   % system bucket maps named instances to they physical reference
   Ref = ets:new(kvs_sys_ref, [public, named_table]),
   ets:insert(kvs_sys_ref, {kvs_sys_ref, kvs_sys_ref}),
   {ok, Ref};  

construct([kvs_sys_bucket]) ->
   % system bucket contains metadata of other buckets
   Ref = ets:new(kvs_sys_bucket, [public, named_table]),
   ets:insert(kvs_sys_ref, {kvs_sys_bucket, kvs_sys_bucket}),
   % register all system buckets (enables access via kvs interface)
   SysRef = [
      {name,    kvs_sys_ref},
      {storage, ?MODULE}
   ],
   SysBkt = [
      {name,    kvs_sys_bucket},
      {storage, ?MODULE}
   ],
   ets:insert(kvs_sys_bucket, {kvs_sys_ref,    SysRef}),
   ets:insert(kvs_sys_bucket, {kvs_sys_bucket, SysBkt}),
   error_logger:info_report(SysRef),
   error_logger:info_report(SysBkt),
   {ok, Ref};
      
construct([Bucket | _]) ->
   Name = proplists:get_value(name, Bucket),
   Ref  = ets:new(anonymous, [public]),
   ok   = kvs:put(kvs_sys_ref, Name, Ref),
   {ok, Ref}.   

%%
%%
config() ->
   [].

%%
%%
put(Name, Key, Item) ->
   case ets:insert(Name, {Key, Item}) of
      true -> ok;
      _    -> {error, fatal} 
   end.   

%%
%%
has(Name, Key) ->
   case kvs_sys:get(Name, Key) of
      {ok, _} -> true;
      _       -> false
   end.

%%
%%
get(Name, Key) ->
   case ets:lookup(Name, Key) of
      [{Key, Item}] -> {ok, Item};
      []            -> {error, not_found}
   end.

%%
%%
remove(Name, Key) ->
   ets:delete(Name, Key),
   ok.


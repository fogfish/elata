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
-module(gen_kvs_entity).
-author(dmitry.kolesnikov@nokia.com).

%%
%% Generic entity (single process per entity)
%%
-export([
   do_create/2,
   do_insert/2,
   do_lookup/2,
   do_delete/2,
   % custom behaviour
   behaviour_info/1
]).

%%%------------------------------------------------------------------
%%%
%%% Behaviour interface
%%%
%%%------------------------------------------------------------------
behaviour_info(callbacks) ->
   [
      {construct, 1}, %% constructs([...])
      {destroy,   1}, %% destroy(Pid)  
      {set,  2},      %% set(Pid, Item)
      {get,  1}       %% get(Pid)
   ];

behaviour_info(_) -> 
   undefined.

%%%------------------------------------------------------------------
%%%
%%% Protected Functions (keyval_store interface)
%%%
%%%------------------------------------------------------------------
   

%%
%%
do_create(Bucket, Item) ->
   Key = key(Bucket, Item),
   case kvs_reg:resolve(Key) of
      {error, _} -> spawn_entity(Bucket, Item), ok;
      {ok, _Pid} -> {error, already_exists}
   end.

%%
%%
do_insert(Bucket, Item) ->
   Key = key(Bucket, Item),
   case kvs_reg:resolve(Key) of
      {error, _} -> 
         spawn_entity(Bucket, Item);
      {ok,  Pid} ->
         Mod = get_plugin(Bucket),
         Mod:set(Pid, Item)
   end.

%%
%%
do_lookup(Bucket, Key) ->
   case kvs_reg:resolve(Key) of
      {ok, Pid}  ->
         Mod = get_plugin(Bucket),
         Mod:get(Pid);
      {error, _} ->
         {error, not_found}
   end.

%%
%%
do_delete(Bucket, Key) ->
   case kvs_reg:resolve(Key) of
      {ok, Pid} ->
         Mod = get_plugin(Bucket),
         Mod:destroy(Pid),
         kvs_reg:unregister(Key),
         ok;
      {error, _} ->
         ok
   end.
   
%%%------------------------------------------------------------------
%%%
%%% Private Functions
%%%
%%%------------------------------------------------------------------
get_plugin(Bucket) ->
   case proplists:get_value(plugin, Bucket) of
      {Mod, _}  -> Mod;
      Mod       -> Mod
   end.

key(Bucket, Item) when is_list(Item) ->
   Key = proplists:get_value(key, Bucket),
   proplists:get_value(Key, Item);
key(Bucket, Item) when is_tuple(Item) ->
   Key = proplists:get_value(key, Bucket),
   erlang:element(Key, Item).
   
spawn_entity(Bucket, Item) ->
   Key = key(Bucket, Item),
   {ok, Pid} = case proplists:get_value(plugin, Bucket) of
      {Mod, Args} -> Mod:construct(Args ++ [Key, Item]);   
      Mod         -> Mod:construct([Key, Item])
   end,
   % Process should register itself at init routine (fault recovery)
   kvs_reg:register(Key, Pid), 
   {ok, Pid}.
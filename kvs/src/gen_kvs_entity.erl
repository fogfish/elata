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
      {factory, 1}, %% factory method create instance
      {set,  2},    %% set(Pid, Entity)
      {get,  1},    %% get(Pid)
      {kill, 1}     %% kill(Pid)
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
do_create(Domain, Entity) ->
   Key = key(Domain, Entity),
   case kvs_reg:resolve(Key) of
      {error, _} -> spawn_entity(Domain, Entity), ok;
      {ok, _Pid} -> {error, already_exists}
   end.

%%
%%
do_insert(Domain, Entity) ->
   Key = key(Domain, Entity),
   case kvs_reg:resolve(Key) of
      {error, _} -> 
         spawn_entity(Domain, Entity);
      {ok,  Pid} ->
         Mod = get_plugin(Domain),
         Mod:set(Pid, Entity)
   end.

%%
%%
do_lookup(Domain, Key) ->
   case kvs_reg:resolve(Key) of
      {ok, Pid}  ->
         Mod = get_plugin(Domain),
         Mod:get(Pid);
      {error, _} ->
         {error, not_found}
   end.

%%
%%
do_delete(Domain, Key) ->
   case kvs_reg:resolve(Key) of
      {ok, Pid} ->
         Mod = get_plugin(Domain),
         Mod:kill(Pid),
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
get_plugin(Domain) ->
   case proplists:get_value(plugin, Domain) of
      {Mod, _}  -> Mod;
      Mod       -> Mod
   end.

key(Domain, Entity) when is_list(Entity) ->
   Key = proplists:get_value(key, Domain),
   proplists:get_value(Key, Entity);
key(Domain, Entity) when is_tuple(Entity) ->
   Key = proplists:get_value(key, Domain),
   erlang:element(Key, Entity).
   
spawn_entity(Domain, Entity) ->
   Key = key(Domain, Entity),
   {ok, Pid} = case proplists:get_value(plugin, Domain) of
      {Mod, Args} -> Mod:factory(Args ++ [Key, Entity]);   
      Mod         -> Mod:factory([Key, Entity])
   end,
   % Process should register itself at init routine (fault recovery)
   kvs_reg:register(Key, Pid), 
   {ok, Pid}.
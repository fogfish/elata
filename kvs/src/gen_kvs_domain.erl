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
-module(gen_kvs_domain).
-author(dmitry.kolesnikov@nokia.com).

%%
%% Generic key/value domain (single process manages all domain entities)
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
      {factory, 1}, %% factory method create instance factory(Args)
      {create,  2}, %% create(Pid, Entity)
      {insert,  2}, %% insert(Pid, Entity)
      {lookup,  2}, %% lookup(Pid, Key)
      {delete,  2}  %% delete(Pid, Key)
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
   Name = proplists:get_value(name, Domain),
   {ok, Pid} = case kvs_reg:resolve(Name) of
      {error, _} -> spawn_domain(Name, Domain);
      R          -> R
   end,
   Mod = get_plugin(Domain),
   Mod:create(Pid, Entity).

%%
%%
do_insert(Domain, Entity) ->
   Name = proplists:get_value(name, Domain),
   {ok, Pid} = case kvs_reg:resolve(Name) of
      {error, _} -> spawn_domain(Name, Domain);
      R          -> R
   end,
   Mod = get_plugin(Domain),
   Mod:insert(Pid, Entity).
   
%%
%%
do_lookup(Domain, Key) ->
   Name = proplists:get_value(name, Domain),
   case kvs_reg:resolve(Name) of
      {ok, Pid}  -> 
         Mod = get_plugin(Domain),
         Mod:lookup(Pid, Key);
      {error, _} ->
         {error, not_found}
   end.

%%
%%
do_delete(Domain, Key) ->
   Name = proplists:get_value(name, Domain),
   case kvs_reg:resolve(Name) of
      {ok, P}  -> 
         Mod = get_plugin(Domain),
         Mod:delete(P, Key);
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
      {Mod, _} -> Mod;
      Mod      -> Mod
   end.

spawn_domain(Name, Domain) ->
   {ok, Pid} = case proplists:get_value(plugin, Domain) of
      {Mod, Args} -> Mod:factory([Name, Domain | Args]);   
      Mod         -> Mod:factory([Name, Domain])
   end,
   % Process should register itself at init routine (fault recovery)
   kvs_reg:register(Name, Pid), 
   {ok, Pid}.
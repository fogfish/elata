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
-module(kvs_cache_sup).
-author(dmitry.kolesnikov@nokia.com).

-behaviour(supervisor).
-behaviour(gen_kvs).

-export([
   % supervisor
   start_link/1,
   init/1,                    
   % gen_kvs_bucket
   put/3,
   has/2,
   get/2,
   remove/2,
   map/2,
   fold/3
]).

%%%------------------------------------------------------------------
%%%
%%% Supervisor
%%%
%%%------------------------------------------------------------------
start_link(Spec) ->
   supervisor:start_link({local, ?MODULE}, ?MODULE, [Spec]).
   
init([Spec]) ->
   Cat     = proplists:get_value(name, Spec), 
   {ok, _} = kvs:new({key, Cat}, [{storage, kvs_sys}]),
   gen_kvs:init(Cat, Spec),
   {ok,
      {
         {simple_one_for_one, 2, 1},   % 2 faults per second
         [element(Cat)]
      }
   }.

element(Cat) ->
   {
      kvs_cache,       % child id
      {
         kvs_cache,  % Mod
         start_link, % Fun
         [Cat]       % Args
      },
      transient, 2000, worker, dynamic 
   }.   
   
%%%------------------------------------------------------------------
%%%
%%% gen_kvs_entity
%%%
%%%------------------------------------------------------------------
put(Cat, Key, Val) ->
   case gen_kvs:key_to_pid(Cat, Key) of
      {error, not_found} ->
         {ok, _} = supervisor:start_child(?MODULE, [Key, Val]),
         ok;
      {ok, Pid}          ->
         gen_server:call(Pid, {kvs_put, Key, Val})
   end.
   
has(Cat, Key) ->
   case gen_kvs:key_to_pid(Cat, Key) of
      {error, not_found} -> false;
      {ok, _}            -> true
   end.
   
get(Cat, Key) ->
   case gen_kvs:key_to_pid(Cat, Key) of
      {ok, Pid} -> gen_server:call(Pid, {kvs_get, Key});
      Error     -> Error
   end.
   
remove(Cat, Key) ->
   case gen_kvs:key_to_pid(Cat, Key) of
      {ok, Pid}          -> gen_server:cast(Pid, {kvs_remove, Key});
      {error, not_found} -> ok
   end.
      
map(Cat, Fun) ->
   gen_kvs:key_map(
      Cat, 
      fun(Pid, Key) -> gen_server:call(Pid, {kvs_get, Key}) end,
      Fun
   ).


fold(Cat, Acc, Fun) ->
   gen_kvs:key_fold(
      Cat,
      Acc,
      fun(Pid, Key) -> gen_server:call(Pid, {kvs_get, Key}) end,
      Fun
   ).
   

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
-include_lib("stdlib/include/qlc.hrl").

%%
%% kvs_sys is a system category
%%

-export([
   start_link/1,
   % public API
   put/2,
   put/3,
   has/2,
   get/2,
   remove/2,
   map/2,
   fold/3
]).

%%
%% debug macro
-ifdef(DEBUG).
-define(DEBUG(M), error_logger:info_report([{?MODULE, self()}] ++ M)).
-else.
-define(DEBUG(M), true).
-endif.

%%
%%
start_link(kvs_sys_ref) ->
   % system bucket maps named instances to they physical reference
   Ref = ets:new(kvs_sys_ref, [public, named_table, ordered_set]),
   ets:insert(kvs_sys_ref, {kvs_sys_ref, {?MODULE, kvs_sys_ref}}),
   {ok, Ref};  

start_link(kvs_sys_cat) ->
   % system bucket contains metadata of other buckets
   Ref = ets:new(kvs_sys_cat, [public, named_table, ordered_set]),
   ets:insert(kvs_sys_ref, {kvs_sys_cat, {?MODULE, kvs_sys_cat}}),
   % register all system buckets (enables access via kvs interface)
   SysRef = [
      {name,    kvs_sys_ref},
      {storage, ?MODULE}
   ],
   SysCat = [
      {name,    kvs_sys_cat},
      {storage, ?MODULE}
   ],
   ets:insert(kvs_sys_cat, {kvs_sys_ref, SysRef}),
   ets:insert(kvs_sys_cat, {kvs_sys_cat, SysCat}),
   ?DEBUG(SysRef),
   ?DEBUG(SysCat),
   {ok, Ref};
      
start_link(Cat) ->
   Name   = proplists:get_value(name,   Cat),
   Type   = proplists:get_value(type,   Cat, ordered_set),
   Access = proplists:get_value(access, Cat, public),
   % % TODO: options
   Ref  = ets:new(kvs_category, [Access, Type]),
   ok   = kvs:put(kvs_sys_ref,  Name, {?MODULE, Ref}),
   ok   = kvs:put(kvs_sys_cat,  Name, Cat),
   {ok, Ref}.   

%%
%%
put(Cat, Val) ->
   ets:insert(Cat, Val),
   ok.

put(Cat, Key, Item) ->    
   kvs_sys:put(Cat, {Key, Item}).   

%%
%%
has(Cat, Key) ->
   case kvs_sys:get(Cat, Key) of
      {ok, _} -> true;
      _       -> false
   end.

%%
%%
get(Cat, Key) ->
   case ets:lookup(Cat, Key) of
      [{Key, Val}] -> {ok, Val};
      [Val]        -> {ok, Val};
      []           -> {error, not_found}
   end.

%%
%%
remove(Cat, Key) ->
   ets:delete(Cat, Key),
   ok.

   
%%
%%
map(Cat, Fun)  ->
   Map = fun({K, V}) -> Fun(K, V) end,
   Q = qlc:q([ Map(X) || X <- ets:table(Cat)]),
   qlc:e(Q).

%%   
%%
fold(Cat, Acc, Fun) ->
   Fold = fun({K, V}, A) -> Fun(K, V, A) end,
   Q = qlc:q([ X || X <- ets:table(Cat)]),
   qlc:fold(Fold, Acc, Q).
   

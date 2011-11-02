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
-module(kvs).
-author(dmitry.kolesnikov@nokia.com).

%%%
%%% Active Key/Value store provides unified interface for key-value storage.
%%% Items are allocated to buckets. 
%%%

-export([
   start/0,
   new/2,
   put/3,
   get/2,
   has/2,
   remove/2,
   map/2,
   fold/3
]).

%%%
%%% Start kvs application
start() ->
   {file, Module} = code:is_loaded(?MODULE),
   AppFile = filename:dirname(Module) ++ "/" ++ atom_to_list(?MODULE) ++ ".app",
   {ok, [{application, _, List}]} = file:consult(AppFile), 
   Apps = proplists:get_value(applications, List, []),
   lists:foreach(
      fun(X) -> application:start(X) end,
      Apps
   ),
   application:start(?MODULE).


%%%
%%% Define a new Key/Value category
%%%
%%% new(Cat, Opts) -> {ok, Pid} | {error, ...}
%%%    Cat  = string() unique name
%%%    Opts = [opt]
%%%       {storage, Module} plugin implementation of gen_kvs_bucket
%%%       {identity,   Fun} Item identity function Fun(Item) -> identity 
%%%       {getter,     Fun} Item getter function   Fun(Attr, Item) -> value 
%%%       event             events are fired when bucket is changed
new(Cat, Opts) ->
   case kvs_sys:get(kvs_sys_ref, Cat) of
      {ok,  Pid} -> 
         {error, {already_exists, Pid}};
      {error, _} ->
         Spec = [{name, Cat} | Opts],
         case proplists:get_value(storage, Spec) of
            kvs_sys -> kvs_sys:start_link(Spec);
            _       -> kvs_sup:start_category(Spec)
         end
   end.   
   
%%%
%%% put(Cat, Key, Item) -> ok | {error, ...}
%%%    Cat = unique name of category
%%%    Val = item to store
%%%    Key = unique item identity (identity function over Item)
%%% 
%%% stores Item into Bucket
put(Cat, Key, Val)  -> 
   case kvs_sys:get(kvs_sys_ref, Cat) of
      {error, _}        -> 
         {error, no_category};
      {ok,  {Mod, Ref}} -> 
         ok = Mod:put(Ref, Key, Val),
         finalize(put, Cat, Key, Val),
         ok
   end.

%%%
%%% has(Bucket, Key) -> true | false
%%%    Bucket = bucket unique name, see kvs_bucket:define(...)
%%%    Key    = unique item identity
%%% check key in the bucket
has(Cat, Key) ->
   case kvs_sys:get(kvs_sys_ref, Cat) of
      {error, _}        -> {error, no_category};
      {ok,  {Mod, Ref}} -> Mod:has(Ref, Key) 
   end.

%%%
%%% get(Bucket, Key) -> {ok, Item} | {error, ...}
%%%    Bucket = bucket unique name, see kvs_bucket:define(...)
%%%    Key    = unique item identity
%%% retrives key from bucket
get(Cat, Key) ->
   case kvs_sys:get(kvs_sys_ref, Cat) of
      {error, _}        -> {error, no_category};
      {ok,  {Mod, Ref}} -> Mod:get(Ref, Key) 
   end.
   
%%%
%%% remove(Bucket, Key) -> ok | {error, ...}
%%%    Bucket = bucket unique name, see kvs_bucket:define(...)
%%%    Key    = unique item identity
%%% removes keys from bucket
remove(Cat, Key) ->
   case kvs_sys:get(kvs_sys_ref, Cat) of
      {error, _}        -> {error, no_category};
      {ok,  {Mod, Ref}} -> 
         ok = Mod:remove(Ref, Key),
         finalize(remove, Cat, Key, undefined)
   end.
 
%%%
%%% map(Bucket, Fun) -> [...] | {error, ...}
%%%    Bucket = bucket unique name, see kvs_bucket:define(...)
%%%    Fun    = Fun(Key, Item) -> Val
%%% maps function over bucket
map(Cat, Fun) ->
   case kvs_sys:get(kvs_sys_ref, Cat) of
      {error, _}        -> {error, no_category};
      {ok,  {Mod, Ref}} -> 
         %% validate if map/2 is supported
         case lists:member({map,2}, Mod:module_info(exports)) of
            true  -> Mod:map(Ref, Fun);
            false -> {error, not_supported}
         end
   end.
   
%%%
%%% fold(Bucket, Acc, Fun) -> [...] | {error, ...}
%%%    Bucket = bucket unique name, see kvs_bucket:define(...)
%%%    Fun    = Fun(Key, Item, Acc) -> Val | undefined
%%% fold function over bucket
fold(Cat, Acc, Fun) ->
   case kvs_sys:get(kvs_sys_ref, Cat) of
      {error, _}        -> {error, no_category};
      {ok,  {Mod, Ref}} -> 
         case lists:member({fold,3}, Mod:module_info(exports)) of
            true  -> Mod:fold(Ref, Acc, Fun);
            false -> {error, not_supported}
         end
   end.

   
%%%------------------------------------------------------------------
%%%
%%% Private Functions
%%%
%%%------------------------------------------------------------------

finalize(Act, Cat, Key, Val) ->
   {ok, Spec} =  kvs_sys:get(kvs_sys_cat, Cat),
   case proplists:is_defined(event, Spec) of
      true  -> kvs_evt:notify(Act, Cat, Key, Val);
      false -> ok
   end.
      





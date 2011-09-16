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
   put/2,
   get/2,
   has/2,
   remove/2
]).


%%%
%%% put(Bucket, Item) -> {ok, Key} | {error, ...}
%%%    Bucket = bucket unique name, see kvs_bucket:define(...)
%%%    Item   = item to store
%%%    Key    = unique item identity (identity function over Item)
%%% 
%%% stores Item into Bucket
put(Bucket, Item)  -> 
   % retrive bucket metadata
   case kvs_sys:get(kvs_sys_bucket, Bucket) of
      {error,  _} -> {error, undefined_bucket};
      {ok, Bmeta} -> gen_kvs_bucket:handle_put(Bmeta, Item)
   end.

%%%
%%% has(Bucket, Key) -> true | false
%%%    Bucket = bucket unique name, see kvs_bucket:define(...)
%%%    Key    = unique item identity
%%% check key in the bucket
has(Bucket, Key) ->
   case kvs_sys:get(kvs_sys_bucket, Bucket) of
      {error,  _} -> {error, undefined_bucket};
      {ok, Bmeta} -> gen_kvs_bucket:handle_has(Bmeta, Key)
   end.

%%%
%%% get(Bucket, Key) -> {ok, Item} | {error, ...}
%%%    Bucket = bucket unique name, see kvs_bucket:define(...)
%%%    Key    = unique item identity
%%% retrives key from bucket
get(Bucket, Key) ->
   case kvs_sys:get(kvs_sys_bucket, Bucket) of
      {error,  _} -> {error, undefined_bucket};
      {ok, Bmeta} -> gen_kvs_bucket:handle_get(Bmeta, Key)
   end.
   
%%%
%%% remove(Bucket, Key) -> ok | {error, ...}
%%%    Bucket = bucket unique name, see kvs_bucket:define(...)
%%%    Key    = unique item identity
%%% removes keys from bucket
remove(Bucket, Key) ->
   case kvs_sys:get(kvs_sys_bucket, Bucket) of
      {error,  _} -> {error, undefined_bucket};
      {ok, Bmeta} -> gen_kvs_bucket:handle_remove(Bmeta, Key)
   end.

%%%------------------------------------------------------------------
%%%
%%% Private Functions
%%%
%%%------------------------------------------------------------------


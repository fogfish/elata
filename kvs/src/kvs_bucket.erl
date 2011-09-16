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
-module(kvs_bucket).
-author(dmitry.kolesnikov@nokia.com).

%%
%% A bucket is a hashtable, it manages items keyspace and
%% own assotiated metadata.
%%

-export([
   define/2,                    
   lookup/1,
   remove/1
]).


%%
%% Define a new Key/Value bucket
%%
%% Name   = string() unique domain name
%% Bucket = [opt]
%%    opt = event | {storage, Module} | {id, Func}
%%    event  = bucket generates events when data is changed
%%    Module = plugin implementation gen_kvs_bucket
%%    Args   = list(), list of arguments supplied to factory method
%%    Func   = Item identity function
%%       sha1 - key is SHA1(Item)
%%       {attr, Name} - key is value of attribute (Name) or position of tuple
%%
define(Name, Bucket) ->
   % TODO: assert Bucket metadata
   case kvs:get(kvs_sys_bucket, Name) of
      {ok,  Pid} ->
         {error, already_exists};
      {error, _} ->
         Mod = proplists:get_value(storage, Bucket),
         Cfg = Mod:config(),
         % supervise storage plug-in if nessesary
         case proplists:is_defined(supervise, Cfg) of
            false -> ok;
            true  -> kvs_sup:attach(Mod)
         end,
         % start-up keyspace management
         case proplists:get_value(keyspace, Cfg) of
            undefined ->
               ok;
            Keyspace  ->
               ok = kvs_bucket:define({keyspace, Name}, [{storage, kvs_sys}, {id, {attr, 1}}])
         end,
         % create bucket instance
         Bmeta = lists:append([[{name, Name}], Cfg, Bucket]),
         {ok, _Pid} = Mod:construct([Bmeta]),
         kvs:put(kvs_sys_bucket, Bmeta),
         error_logger:info_report(Bmeta),
         ok
   end.

%%
%%
lookup(Name) ->
   kvs:get(kvs_sys_bucket, Name).

%%
%% Delete definition of storage domain (domain entities are not impacted)
remove(Name) ->
   % TODO: destroy bucket processes
   kvs:remove(kvs_sys_bucket, Name).
   
%%%------------------------------------------------------------------
%%%
%%% Private Functions
%%%
%%%------------------------------------------------------------------


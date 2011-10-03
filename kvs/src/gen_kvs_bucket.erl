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
-module(gen_kvs_bucket).
-author(dmitry.kolesnikov@nokia.com).

%%
%% Bucket is a hashtable, composed of
%%  - metadata defines bucket operational parameters
%%  - keyspace assotiation table that maps logical key to entity addresses (active bucket)
%%  - storage  persists key/values

%% Generic bucket controller own bucket meta-data and keyspace
%%
%%  - static bucket: single bucket process manages whole keyspace (e.g. proxy)
%%  - active bucket: each item is managed by dedicated process (e.g. worker) 
%%


%%
%% Static key/value bucket, 
%% (Used as a proxy to exised persistent/in-memory storage)
%%
-export([
   handle_put/3,
   handle_has/2,
   handle_get/2,
   handle_remove/2,
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
      {construct, 1}, %% construct([...])
      {config,    0}, %% bucket configuration
      {put,  3},      %% set(Pid, Key, Item)
      {has,  2},      %% has(Pid, Key)                       
      {get,  2},      %% get(Pid, Key)
      {remove, 2}     %% remove(Pid, Key)
   ];

behaviour_info(_) -> 
   undefined.
   
%%%------------------------------------------------------------------
%%%
%%% Protected Functions (kvs interface)
%%%
%%%------------------------------------------------------------------
handle_put(Bucket, Key, Item) ->
   Bid = proplists:get_value(name,    Bucket),
   Mod = proplists:get_value(storage, Bucket),
   % calculate item identity
   % IdF = proplists:get_value(id, Bucket),
   % Key = identity(IdF, Item),
   case resolve(Bid, Key) of
      undefined -> 
         {ok, _Pid} = Mod:construct([Bucket, Key, Item]);
      Pid       -> 
         ok = Mod:put(Pid, Key, Item)
   end,
   notify(put, Bucket, Key, Item),
   ok.

handle_has(Bucket, Key) ->
   Bid = proplists:get_value(name,    Bucket),
   Mod = proplists:get_value(storage, Bucket),
   case kvs_sys:get(kvs_sys_ref, {keyspace, Bid}) of
      {error, not_found} ->
         case kvs_sys:get(kvs_sys_ref, Bid) of
            {error, not_found} -> false;
            {ok, Pid}          -> Mod:has(Pid, Key)
         end;
      {ok, Keyspace}     ->
         % (keyspace partitioned) entity storage is resolved via bucket specific keyspace
         case kvs_sys:get(Keyspace, Key) of
            {error, not_found} -> false;
            {ok, Pid}          -> Mod:has(Pid, Key)
         end
   end.
   
handle_get(Bucket, Key) ->
   Bid = proplists:get_value(name,    Bucket),
   Mod = proplists:get_value(storage, Bucket),
   case resolve(Bid, Key) of
      undefined -> {error, not_found};
      Pid       -> Mod:get(Pid, Key)
   end.

handle_remove(Bucket, Key) ->
   Bid = proplists:get_value(name,    Bucket),
   Mod = proplists:get_value(storage, Bucket),
   case resolve(Bid, Key) of
      undefined -> ok;
      Pid       -> Mod:remove(Pid, Key)
   end,
   notify(remove, Bucket, Key, undefined),
   ok.
   
%%%------------------------------------------------------------------
%%%
%%% Bucket helper methods
%%%
%%%------------------------------------------------------------------   
%%%
%%% Items identity function
%%%

%
%identity(sha1, Item) ->
%   crypto:sha(erlang:term_to_binary(Item));
% 
%identity({attr, Key}, Item) when is_tuple(Item) ->
%   erlang:element(Key, Item);
%
%identity({attr, Key}, Item) when is_list(Item) ->
%   proplists:get_value(Key, Item). 
%   

%%%------------------------------------------------------------------
%%%
%%% Private Functions
%%%
%%%------------------------------------------------------------------

% resolve Pid of handler responsible for given key   
resolve(Bucket, Key) ->
   case kvs_sys:get(kvs_sys_ref, {keyspace, Bucket}) of
      {error, not_found}   ->
         case kvs_sys:get(kvs_sys_ref, Bucket) of
            {error, not_found} -> 
               error_logger:error_msg('KVS: access to undefined bucket ~p~n', [Bucket]),
               undefined;
            {ok, Pid}          -> 
               Pid
         end;
      {ok, Keyspace}      ->
         % (keyspace partitioned) entity storage is resolved via bucket specific keyspace
         case kvs_sys:get(Keyspace, Key) of
            {error, not_found} -> undefined;
            {ok,    Pid}       -> Pid
         end
   end.
   
%%%
%%%  Notify
%%%
notify(put, Bucket, Key, Item) ->
   case proplists:is_defined(event, Bucket) of
      true  -> 
         Bname = proplists:get_value(name, Bucket),
         kvs_evt:put(Bname, Key, Item);
      false -> 
         ok
   end;

notify(remove, Bucket, Key, Item) ->
   case proplists:is_defined(event, Bucket) of
      true  -> 
         Bname = proplists:get_value(name, Bucket),
         kvs_evt:remove(Bname, Key, Item);
      false -> 
         ok
   end;
   
notify(_, _, _, _) -> 
   ok.      
   

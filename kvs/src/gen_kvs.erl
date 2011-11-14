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
-module(gen_kvs).
-author(dmitry.kolesnikov@nokia.com).
-include_lib("stdlib/include/qlc.hrl").

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
   % plugin utility api
   init/1,
   init/2,
   terminate/1,
   key_to_pid/2,
   vinit/2,
   vterminate/2,
   vattr/3,
   key_map/3,
   key_fold/4,
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
      {put,  3},      %% put(Pid, Key, Item) -> ok | {error, ...}
      {has,  2},      %% has(Pid, Key)  -> true | false                     
      {get,  2},      %% get(Pid, Key)  -> {ok, Item} | {error, ...}
      {remove, 2},    %% remove(Pid, Key) -> ok | {error, ...}
      {map,    2},    %% map(Pid, Fun) -> Result | {error, ...}
      {fold,   3}     %% fold(Pid, Acc, Fun) -> Result | {error, ...}
   ];

behaviour_info(_) -> 
   undefined.
   
%%%------------------------------------------------------------------
%%%
%%% Utility interface
%%%
%%%------------------------------------------------------------------
   
%%
%%
init(Spec) ->
   init(self(), Spec).
init(Pid, Spec) ->
   Name   = proplists:get_value(name,    Spec),
   Mod    = proplists:get_value(storage, Spec),
   % enable data federation for the category
   NSpec = case proplists:get_value(fed, Spec) of
      undefined ->
         Spec;
      true      ->
         Uri = "/kvs/fed/" ++ bin_to_hex(crypto:sha(term_to_binary(Name))),
         create_fed(Uri, Name),
         [{fed, Uri} | proplists:delete(fed, Spec)];
      Uri       ->
         create_fed(Uri, Name),
         Spec
   end,
   ok     = kvs:put(kvs_sys_ref, Name, {Mod, Pid}),
   ok     = kvs:put(kvs_sys_cat, Name, NSpec).

%%
%%
terminate(Spec) ->   
   Name = proplists:get_value(name, Spec),
   kvs:remove(kvs_sys_ref, Name),
   ok.
   
%%   
%% key_to_pid(Cat, Key) -> {ok, Pid} | {error, ...} 
%%
key_to_pid(Cat, Key) ->
   {ok, {Mod, Ref}} = kvs:get(kvs_sys_ref, {key, Cat}),
   Mod:get(Ref, Key).

%%
%% register key
vinit(Cat, Key) ->
   ok = kvs:put({key, Cat}, Key, self()).
   
vterminate(Cat, Key) ->
   ok = kvs:remove({key, Cat}, Key).

%%
%%
%%
vattr(Cat, Attr, Val) ->
   {ok, Spec} = kvs:get(kvs_sys_cat, Cat),
   case proplists:get_value(getter, Spec) of
      undefined -> undefined;
      Get       -> Get(Attr, Val)
   end.
   
%%
%%
%%
key_map(Cat, Get, Fun) ->
   kvs:map(
      {key, Cat}, 
      fun(Key, Pid) ->
         {ok, Val} = Get(Pid, Key),
         Fun(Key, Val)
      end
   ).

key_fold(Cat, Acc, Get, Fun) ->
   kvs:fold(
      {key, Cat},
      Acc,
      fun(Key, Pid, Acc1) ->
         {ok, Val} = Get(Pid, Key),
         Fun(Key, Val, Acc1)
      end
   ).

   
%%%------------------------------------------------------------------
%%%
%%% Protected Functions (kvs interface)
%%%
%%%------------------------------------------------------------------

% 
% handle_has(Bucket, Key) ->
   % Bid = proplists:get_value(name,    Bucket),
   % Mod = proplists:get_value(storage, Bucket),
   % case kvs_sys:get(kvs_sys_ref, {keyspace, Bid}) of
      % {error, not_found} ->
         % case kvs_sys:get(kvs_sys_ref, Bid) of
            % {error, not_found} -> false;
            % {ok, Pid}          -> Mod:has(Pid, Key)
         % end;
      % {ok, Keyspace}     ->
         % % (keyspace partitioned) entity storage is resolved via bucket specific keyspace
         % case kvs_sys:get(Keyspace, Key) of
            % {error, not_found} -> false;
            % {ok, Pid}          -> Mod:has(Pid, Key)
         % end
   % end.
   % 
% handle_get(Bucket, Key) ->
   % Bid = proplists:get_value(name,    Bucket),
   % Mod = proplists:get_value(storage, Bucket),
   % case resolve(Bid, Key) of
      % undefined -> {error, not_found};
      % Pid       -> Mod:get(Pid, Key)
   % end.
% 
% handle_remove(Bucket, Key) ->
   % Bid = proplists:get_value(name,    Bucket),
   % Mod = proplists:get_value(storage, Bucket),
   % case resolve(Bid, Key) of
      % undefined -> ok;
      % Pid       -> Mod:remove(Pid, Key)
   % end,
   % notify(remove, Bucket, Key, undefined),
   % ok.
   % 
% handle_map(Bucket, Fun) ->
   % case lists:member(map, proplists:get_value(feature, Bucket, [])) of
      % false ->
         % {error, not_supported};
      % true  ->
         % Bid = proplists:get_value(name,    Bucket),
         % Mod = proplists:get_value(storage, Bucket),
         % % resolve keyspace management
         % case kvs_sys:get(kvs_sys_ref, {keyspace, Bid}) of
            % {error, not_found}  ->
               % % no key space management
               % {ok, Pid} = kvs_sys:get(kvs_sys_ref, Bid),
               % Mod:map(Pid, Fun);
            % {ok, Keyspace}      ->
               % % there is a key space management process
               % kvs_sys:map(Keyspace, 
                  % fun(Key, Pid) ->
                     % {ok, Item} = Mod:get(Pid, Key),
                     % Fun(Key, Item)
                  % end
               % )
         % end
   % end.
   % 
% handle_fold(Bucket, Acc, Fun) ->
   % case lists:member(map, proplists:get_value(feature, Bucket, [])) of
      % false ->
         % {error, not_supported};
      % true  ->
         % Bid = proplists:get_value(name,    Bucket),
         % Mod = proplists:get_value(storage, Bucket),
         % % resolve keyspace management
         % case kvs_sys:get(kvs_sys_ref, {keyspace, Bid}) of
            % {error, not_found}  ->
               % % no key space management
               % {ok, Pid} = kvs_sys:get(kvs_sys_ref, Bid),
               % Mod:fold(Pid, Acc, Fun);
            % {ok, Keyspace}      ->
               % % there is a key space management process
               % kvs_sys:fold(Keyspace, Acc,
                  % fun(Key, Pid, AccIn) ->
                     % {ok, Item} = Mod:get(Pid, Key),
                     % case Fun(Key, Item) of
                        % undefined -> AccIn;
                        % AccOut    -> AccOut
                     % end
                  % end
               % )
         % end
   % end.
   % 
 

%%%------------------------------------------------------------------
%%%
%%% Private Functions
%%%
%%%------------------------------------------------------------------

create_fed(Uri, Name) ->
   case ek:whereis(Uri) of
      undefined -> 
         {ok, _} = kvs_fed_cat_sup:create(Uri, Name);
      _         -> 
         ok
    end.


%%   
%%   
bin_to_hex(Bin) ->
   bin_to_hex(Bin, "").
   
bin_to_hex(<<>>, Acc) ->
   Acc;
bin_to_hex(<<X:8, T/binary>>, Acc) ->  
   bin_to_hex(T, Acc ++ [to_hex(X div 16), to_hex(X rem 16)]).
   
to_hex(X) when X < 10 ->
   $0 + X;
to_hex(X) ->
   $a + (X - 10).


% % resolve Pid of handler responsible for given key   
% resolve(Bucket, Key) ->
   % case kvs_sys:get(kvs_sys_ref, {keyspace, Bucket}) of
      % {error, not_found}   ->
         % case kvs_sys:get(kvs_sys_ref, Bucket) of
            % {error, not_found} -> 
               % error_logger:error_msg('KVS: access to undefined bucket ~p~n', [Bucket]),
               % undefined;
            % {ok, Pid}          -> 
               % Pid
         % end;
      % {ok, Keyspace}      ->
         % % (keyspace partitioned) entity storage is resolved via bucket specific keyspace
         % case kvs_sys:get(Keyspace, Key) of
            % {error, not_found} -> undefined;
            % {ok,    Pid}       -> Pid
         % end
   % end.
   % 
% %%%
% %%%  Notify
% %%%
% notify(put, Bucket, Key, Item) ->
   % case proplists:is_defined(event, Bucket) of
      % true  -> 
         % %Bname = proplists:get_value(name, Bucket),
         % %kvs_evt:put(Bname, Key, Item);
         % kvs_evt:put(Bucket, Key, Item);
      % false -> 
         % ok
   % end;
% 
% notify(remove, Bucket, Key, Item) ->
   % case proplists:is_defined(event, Bucket) of
      % true  -> 
         % %Bname = proplists:get_value(name, Bucket),
         % %kvs_evt:remove(Bname, Key, Item);
         % kvs_evt:remove(Bucket, Key, Item);
      % false -> 
         % ok
   % end;
   % 
% notify(_, _, _, _) -> 
   % ok.      
   % 

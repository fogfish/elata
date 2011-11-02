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
-module(kvs_ets).
-behaviour(gen_kvs).
-behaviour(gen_server).
-author(dmitry.kolesnikov@nokia.com).
-include_lib("stdlib/include/qlc.hrl").

%%
%% In-Memory bucket (uses ETS internally)
%%

-export([
   start_link/1,
   % gen_kvs
   put/3,
   has/2,
   get/2,
   remove/2,
   map/2,
   fold/3,
   % gen_server
   init/1, 
   handle_call/3,
   handle_cast/2, 
   handle_info/2, 
   terminate/2, 
   code_change/3
]).

%%
%% debug macro
-ifdef(DEBUG).
-define(DEBUG(M), error_logger:info_report([{?MODULE, self()}] ++ M)).
-else.
-define(DEBUG(M), true).
-endif.

-record(srv, {
   ref,  % ref to table instance
   cat   % category
}).

%%
%% In-memory key/val storage
%%    Storage specific options
%%       {type,   ...} type, (default set)
%%       {scope,  ...} scope (default private)
start_link(Spec) ->
   gen_server:start_link(?MODULE, [Spec], []).

init([Spec]) ->
   Type   = proplists:get_value(type,  Spec, ordered_set),
   Access = proplists:get_value(scope, Spec, public),
   Ref    = ets:new(kvs_bucket, [Access, Type]),
   gen_kvs:init(Spec),
   ?DEBUG(Spec),
   {ok, 
      #srv{
         ref = Ref,
         cat = Spec
      }
   }. 

%%%------------------------------------------------------------------   
%%%
%%% gen_kvs
%%%
%%%------------------------------------------------------------------

%%
%%
put(Pid, Key, Val) ->
   gen_server:call(Pid, {kvs_put, Key, Val}).

kvs_put(Key, Val, S) ->
   ets:insert(S#srv.ref, {Key, Val}),
   ok.

%%
%%
has(Pid, Key) ->
   gen_server:call(Pid, {kvs_has, Key}).

kvs_has(Key, S) ->
   case ets:lookup(S#srv.ref, Key) of
      [Val] -> true;
      []    -> false
   end.   
   
%%
%%
get(Pid, Key) ->   
   gen_server:call(Pid, {kvs_get, Key}).
  
kvs_get(Key, S) ->
   case ets:lookup(S#srv.ref, Key) of
      [{Key, Val}] -> {ok, Val};
      []           -> {error, not_found}
   end.    
   
%%
%%
remove(Pid, Key) ->
   gen_server:call(Pid, {kvs_remove, Key}).
  
kvs_remove(Key, S) ->  
   ets:delete(S#srv.ref, Key),
   ok.   

%%
%%
map(Pid, Fun)  ->
   gen_server:call(Pid, {kvs_map, Fun}).

kvs_map(Fun, S) ->
   Map = fun({K, V}) -> Fun(K, V) end,
   Q = qlc:q([ Map(X) || X <- ets:table(S#srv.ref)]),
   qlc:e(Q). 

%%   
%%
fold(Pid, Acc, Fun) ->
   gen_server:call(Pid, {kvs_fold, Acc, Fun}).
   
kvs_fold(Acc, Fun, S) ->
   Fold = fun({K, V}, A) -> Fun(K, V, A) end,
   Q = qlc:q([ X || X <- ets:table(S#srv.ref)]),
   qlc:fold(Fold, Acc, Q).

%%%------------------------------------------------------------------   
%%%
%%% gen_server
%%%
%%%------------------------------------------------------------------

handle_call({kvs_put, Key, Val}, _, S) ->
   {reply, catch(kvs_put(Key, Val, S)), S};
handle_call({kvs_has, Key}, _, S) ->
   {reply, kvs_has(Key, S), S};
handle_call({kvs_get, Key}, _, S) ->
   {reply, kvs_get(Key, S), S}; 
handle_call({kvs_remove, Key}, _, S) ->
   {reply, kvs_remove(Key, S), S};
handle_call({kvs_map, Fun}, _, S) ->
   {reply, kvs_map(Fun, S), S};
handle_call({kvs_fold, Acc, Fun}, _, S) ->
   {reply, kvs_fold(Acc, Fun, S), S};
handle_call(_Req, _From, State) ->
   {reply, undefined, State}.

handle_cast(_Req, State) ->
   {noreply, State}.   

handle_info(_Msg, State) ->
   {noreply, State}.   
   
terminate(_Reason, S) ->
   gen_kvs:terminate(S#srv.cat).
   
code_change(_OldVsn, State, _Extra) ->
   {ok, State}.        
   
%%%------------------------------------------------------------------   
%%%
%%% Private functions
%%%
%%%------------------------------------------------------------------


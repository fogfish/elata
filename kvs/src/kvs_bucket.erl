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
-module(kvs_bucket).
-behaviour(gen_server).
-author(dmitry.kolesnikov@nokia.com).

%%
%% ELATA: Adapter to Key/Value storage (ets, dets, ...)   
%%

%%
%% API
-export([
   start_link/2,
   %% crud   
   create/2,
   insert/2,
   lookup/2,
   delete/2,
   %% export
   to_list/1,
   %% gen_server
   init/1, 
   handle_call/3,
   handle_cast/2, 
   handle_info/2, 
   terminate/2, 
   code_change/3 
]).

-record(srv, {
   mod,  %% storage engine
   ref   %% reference to table instance
}).

%%
%% start
start_link(Bucket, Opts) ->
   gen_server:start_link({local, Bucket}, ?MODULE, [Bucket, Opts], []).

init([Bucket, Opts]) ->
   %% Storage
   Mod = proplists:get_value(module, Opts, ets),
   Cfg = proplists:delete(module, Opts),
   open_table(Mod, Bucket, [{keypos, 2} | Cfg]),
   kvs_reg:register(Bucket, self()),
   {ok,
      #srv{
         mod=Mod,
         ref=Bucket
      }
   }.
   
%%
%% Create a new key, error is key exists
create(Pid, Value) ->
   gen_server:call(Pid, {create, Value}).
impl_create(State, Value) ->   
   try
      case apply(State#srv.mod, insert_new, [State#srv.ref, Value]) of
         true  ->
            ok;
         false ->   
            {error, already_exists}
      end   
   catch 
      _Class:Error ->
         {error, Error}
   end.

%%   
%% Insert a value
insert(Pid, Value) ->
   gen_server:call(Pid, {insert, Value}).
impl_insert(State, Value) ->
   try
      case apply(State#srv.mod, insert, [State#srv.ref, Value]) of
         true  -> ok;
         false -> {error, already_exists};
         Ret   -> Ret
      end
   catch 
      _Class:Error ->
         {error, Error}
   end.
   
%%
%% Get a key
lookup(Pid, Key) ->
   gen_server:call(Pid, {lookup, Key}).
impl_lookup(State, Key) ->
   try
      case apply(State#srv.mod, lookup, [State#srv.ref, Key]) of
         [Value] ->
            {ok, Value};
         []      ->
            {error, not_found}
      end
   catch
      _Class:Error ->
         {error, Error}
   end.

%%
%% Delete
delete(Pid, Key) ->
   gen_server:call(Pid, {delete, Key}).
impl_delete(State, Key) ->
   try
      apply(State#srv.mod, delete, [State#srv.ref, Key]),
      ok
   catch
      _Class:Error ->
         {error, Error}
   end.   

%%
%%
to_list(Pid) ->
   gen_server:call(Pid, to_list).
impl_to_list(State) ->
   try
      L = apply(State#srv.mod, match_object, [State#srv.ref, '$1']),
      {ok, L}
   catch
      _Class:Error ->
         {error, Error}
   end.   

   
handle_call({create, Value}, _From, State) ->
   {reply, impl_create(State, Value), State};
handle_call({insert, Value}, _From, State) ->
   {reply, impl_insert(State, Value), State};
handle_call({lookup, Key}, _From, State) ->
   {reply, impl_lookup(State, Key), State};
handle_call({delete, Key}, _From, State) ->
   {reply, impl_delete(State, Key), State};
handle_call(to_list, _From, State) ->
   {reply, impl_to_list(State), State};
handle_call(_Req, _From, State) ->
   {reply, undefined, State}.
handle_cast(_Req, State) ->
   {noreply, State}.

handle_info(_Msg, State) ->
   {noreply, State}.
   
terminate(_Reason, _State) ->
   %kvs_reg:unregister(State#srv.ref),
   ok.
   
code_change(_OldVsn, State, _Extra) ->
   {ok, State}.   
   
%%%------------------------------------------------------------------   
%%%
%%% Private functions
%%%
%%%------------------------------------------------------------------
open_table(ets,  Name, Opts) ->
   ets:new(Name, [set, private, named_table] ++ Opts);
open_table(dets, Name, Opts) ->  
   {ok, _} = dets:open_file(Name, Opts).
   
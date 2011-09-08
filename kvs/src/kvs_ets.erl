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
-behaviour(gen_server).
-author(dmitry.kolesnikov@nokia.com).

%%
%%  Wrapper of Key/Value Interface to ETS
%%

-export([
   start_link/2,
   %% gen_server
   init/1, 
   handle_call/3,
   handle_cast/2, 
   handle_info/2, 
   terminate/2, 
   code_change/3 
]).

%% private server state record
-record(srv, {ref, key}).

%%
%%
start_link(Name, Domain) ->
  gen_server:start_link({local, Name}, ?MODULE, [Name, Domain], []).
  
init([Name, Domain]) ->
   Key  = proplists:get_value(key, Domain),
   Opts = case erlang:is_integer(Key) of
      false -> [set, private, named_table, {keypos, 2}];
      true  -> [set, private, named_table, {keypos, Key}]
   end,   
   ets:new(Name, Opts),
   kvs_reg:register(Name, self()),
   {ok, #srv{ref=Name, key=Key}}.

   
handle_call({create, Entity}, _From, #srv{ref=Ref, key=Key} = State) ->
   {reply, create(Entity, Key, Ref), State};
handle_call({insert, Entity}, _From, #srv{ref=Ref, key=Key} = State) ->
   {reply, insert(Entity, Key, Ref), State};
handle_call({lookup, Key}, _From, #srv{ref=Ref} = State) ->
   {reply, lookup(Key, Ref), State};
handle_call({delete, Key}, _From, #srv{ref=Ref} = State) ->
   {reply, delete(Key, Ref), State};
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

%%
%% create a new entity
create(Entity, _PKey, Table) when is_tuple(Entity) ->
   case ets:insert_new(Table, Entity) of
      true  -> ok;
      false -> {error, already_exists}
   end;

create(Entity, PKey, Table) when is_list(Entity) ->
   Key = proplists:get_value(PKey, Entity),  
   case ets:insert_new(Table, {entity, Key, Entity}) of
      true  -> ok;
      false -> {error, already_exists}
   end;

create(_Entity, _PKey, _Table) ->
   {error, not_supported}.

%%
%% insert entity
insert(Entity, _PKey, Table) when is_tuple(Entity) ->
   case ets:insert(Table, Entity) of
      true -> ok
   end;
   
insert(Entity, PKey, Table) when is_list(Entity) ->
   Key = proplists:get_value(PKey, Entity),
   case ets:insert(Table, {entity, Key, Entity}) of
      true -> ok
   end;
   
insert(_Entity, _PKey, _Table) ->
   {error, not_supported}.
   
   
%%
%% lookup
lookup(Key, Table) ->
   case ets:lookup(Table, Key) of
      [{entity, _, Entity}] -> {ok, Entity};
      [Entity]              -> {ok, Entity};
      []                    -> {error, not_found}
   end.
      
%%
%% delete
delete(Key, Table) ->
   ets:delete(Table, Key),
   ok.
   
   
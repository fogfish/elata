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
-module(keyval_store).
-author(dmitry.kolesnikov@nokia.com).

%%
%% Key/Value Store provides CRUD interface to manage key/val entities   
%% Interface operates with 'entities' (E) that is set of key-values.
%%
%% E {(k,v): k - term(), v - any()}
%%
%% Run-time entity representation implies either list of 
%% attribute-value pairs or erlang tuples/record. The storage domain cannot 
%% mix lists or tuples, one of them should be chosen.
%%

-export([
   create/2,
   insert/2,
   lookup/2,
   lookup/3,
   delete/2
]).

%%
%% TODO guarg condition
%% is_tuple
%% is_list on CRUD operations


%%
%% create new entity
create(Ns, Entity) ->
   case keyval_domain:lookup(Ns) of
      {error, _}  ->
         % domain is not declared
         {error, undefined_domain};
      {ok, Domain} ->
         % domain is found, its config is returned
         Result = case proplists:is_defined(singleton, Domain) of
            % domain is managed by single process
            true  -> gen_kvs_domain:do_create(Domain, Entity);
            % process per entity mode
            false -> gen_kvs_entity:do_create(Domain, Entity)
         end,
         notify(create, Result, Ns, Entity, Domain),
         Result
   end.   

   
%%
%% insert/update entity
insert(Ns, Entity) ->
   case keyval_domain:lookup(Ns) of
      {error, _}  ->
         % domain is not declared
         {error, undefined_domain};
      {ok, Domain} ->
         % domain is found, its config is returned
         Result = case proplists:is_defined(singleton, Domain) of
            % domain is managed by single process
            true  -> gen_kvs_domain:do_insert(Domain, Entity);
            % process per entity mode
            false -> gen_kvs_entity:do_insert(Domain, Entity)
         end,
         notify(insert, Result, Ns, Entity, Domain),
         Result
   end.


%%
%% read value
lookup(Ns, Key) ->
   case keyval_domain:lookup(Ns) of
      {error, _}  ->
         % domain is not declared
         {error, undefined_domain};
      {ok, Domain} ->
         % domain is found, its config is returned
         Result = case proplists:is_defined(singleton, Domain) of
            % domain is managed by single process
            true  -> gen_kvs_domain:do_lookup(Domain, Key);
            % process per entity mode
            false -> gen_kvs_entity:do_lookup(Domain, Key)
         end,
         notify(lookup, Result, Ns, Key, Domain),
         Result
   end.

   
%% 
%% read value with default value
lookup(Ns, Key, Default) ->
   case keyval_store:lookup(Ns, Key) of
      {error, _} ->
         {ok, Default};
      Result ->
         Result
   end.
   
%%
%% delete key from bucket       
delete(Ns, Key) ->
   case keyval_domain:lookup(Ns) of
      {error, _}  ->
         % domain is not declared
         {error, undefined_domain};
      {ok, Domain} ->
         % domain is found, its config is returned
         Result = case proplists:is_defined(singleton, Domain) of
            % domain is managed by single process
            true  -> gen_kvs_domain:do_delete(Domain, Key);
            % process per entity mode
            false -> gen_kvs_entity:do_delete(Domain, Key)
         end,
         notify(delete, Result, Ns, Key, Domain),
         Result
   end.

   
%%
%% export to list
%to_list(Bucket) ->
%   case kvs_reg:resolve(Bucket) of
%      {error, _} ->
%         {error, undefined};
%      {ok,  Pid} ->
%         kvs_bucket:to_list(Pid)
%   end.
%   
%%
%% Import form file
%import(Bucket, Filename) ->
%   {ok, List} = file:consult(Filename),
%   keyval_store:create(Bucket, List).
   
   
%%%------------------------------------------------------------------
%%%
%%% Private Functions
%%%
%%%------------------------------------------------------------------
notify(create, ok, Ns, Entity, Domain) ->
   case proplists:is_defined(event, Domain) of
      true  -> kvs_evt:create(Ns, Entity);
      false -> ok
   end;

notify(insert, ok, Ns, Entity, Domain) ->
   case proplists:is_defined(event, Domain) of
      true  -> kvs_evt:insert(Ns, Entity);
      false -> ok
   end;

%%% disabled due to performance implication  
%%%notify(lookup, {ok, _}, Ns, Key, Domain) ->
%%%   case proplists:is_defined(event, Domain) of
%%%      true  -> kvs_evt:lookup(Ns, Key);
%%%      false -> ok
%%%   end;   

notify(delete, ok, Ns, Key, Domain) ->
   case proplists:is_defined(event, Domain) of
      true  -> kvs_evt:delete(Ns, Key);
      false -> ok
   end;
   
notify(_, _, _, _, _) -> 
   ok.

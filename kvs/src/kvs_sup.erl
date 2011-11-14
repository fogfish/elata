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
-module(kvs_sup).
-behaviour(supervisor).
-author(dmitry.kolesnikov@nokia.com).

-export([
   % supervisor
   start_link/1,
   init/1,
   % custom
   start_category/1
]).

%%%
%%% Root supervisor of Key/Value storage
%%%
%%% Statically handles
%%%  - event manager
%%%  - event handler factory/supervisor
%%%  - kvs_cache (in-memory cache)
%%% 
%%% Dynamically handles
%%%  - storage plug-in defined by application via keyval_bucket:create
%%%

start_link(Config) ->
   supervisor:start_link({local, ?MODULE}, ?MODULE, [Config]).
   
init([Config]) ->   
   {ok,
      {
         {one_for_one, 4, 1800},
         event(Config, 
            factory(Config,
               fed(Config, 
                  synctx(Config, [])
               )
            )
         )
      }
   }.

%%
%%
fed(Config, Acc) ->
   case proplists:is_defined(cluster, Config) of
      true -> Acc ++ [{
         kvs_fed_cat_sup,
         {
            kvs_fed_cat_sup,
            start_link,
            []
         },
         permanent, brutal_kill, supervisor, dynamic
      }];
      false -> Acc
   end.
   
%% Sync TX listener
synctx(Config, Acc) ->
   case proplists:is_defined(cluster, Config) of
      true  -> Acc ++ [{
         kvs_sync_tx,
         {
            kvs_sync_ht_tx,
            start_link,
            []
         },
         permanent, brutal_kill, worker, dynamic
      }, {
         kvs_sync_tx_sup,
         {
            kvs_sync_ht_tx_sup,
            start_link,
            []
         },
         permanent, brutal_kill, supervisor, dynamic
      }];
      false -> Acc
   end.   
   
   
%%
%%
event(Config, Acc) ->  
   Acc ++ [{
      kvs_evt,
      {
         kvs_evt,
         start_link,
         []
      },
      permanent, 2000, worker, dynamic
   }].   

%%   
%%   
factory(Config, Acc) ->
   Acc ++ [{
      kvs_evt_sup,
      {
         kvs_evt_sup,
         start_link,
         []
      },
      permanent, 2000, worker, dynamic
   }].
   
%%%   
%%% Dynamically start bucket adds a module into supervisor tree (called via kvs_define)
%%%
start_category(Cat) ->
   Name = proplists:get_value(name,    Cat),
   Mod  = proplists:get_value(storage, Cat),
   % check module type
   TypeOf = fun(T, M) ->
      lists:member(T, 
         lists:flatten(
            proplists:get_all_values(
               behaviour,  
               M:module_info(attributes)
            )
         )
      )
   end,
   % define child spec
   Type = case TypeOf(supervisor, Mod) of
      true  -> supervisor;
      false -> worker
   end,
   Child = {  % child spec
      Name,
      {
         Mod,                     
         start_link, 
         [Cat]
      },
      permanent, 2000, Type, dynamic
   },
   case supervisor:start_child(kvs_sup, Child) of 
      {ok, Pid}    -> {ok, Pid};
      {ok, Pid, _} -> {ok, Pid};
      {error,{already_started, Pid}} -> {ok, Pid}
   end.   


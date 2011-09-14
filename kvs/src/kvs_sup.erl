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
   start_link/0,
   init/1,
   % custom
   attach/1
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

start_link() ->
   supervisor:start_link({local, ?MODULE}, ?MODULE, []).
   
init([]) ->   
   ok = kvs_reg:start(),
   % plugins
   Cache = {
      kvs_cache_sup,
      {
         kvs_cache_sup,
         start_link,
         []
      },
      permanent, 2000, supervisor, dynamic
   },
   % event management
   EvtManager = {
      kvs_evt,
      {
         kvs_evt,
         start_link,
         []
      },
      permanent, 2000, worker, dynamic
   },
   EvtFactory = {
      kvs_evt_sup,
      {
         kvs_evt_sup,
         start_link,
         []
      },
      permanent, 2000, worker, dynamic
   },
   {ok,
      {
         {one_for_one, 4, 1800},
         [Cache, EvtManager, EvtFactory]
      }
   }.
   
   
%%%   
%%% Attaches a module into supervisor tree
%%%
attach(Mod) ->
   case Mod of
      kvs_cache_sup -> 
         ok;
      _             ->
         Child = {  % child spec
            Mod,
            {
               Mod,
               start_link,
               []
            },
            permanent, 2000, supervisor, dynamic
         },
         case supervisor:start_child(kvs_sup, Child) of 
            {ok, _}    -> ok;
            {ok, _, _} -> ok;
            {error, already_present}     -> ok;
            {error,{already_started, _}} -> ok;
            Err -> Err
         end     
   end.
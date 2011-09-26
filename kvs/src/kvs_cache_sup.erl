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
-module(kvs_cache_sup).
-author(dmitry.kolesnikov@nokia.com).

-behaviour(supervisor).
-behaviour(gen_kvs_bucket).

-export([
   % supervisor
   start_link/1,
   init/1,
   % gen_kvs_bucket
   construct/1,
   config/0,
   put/3,
   has/2,
   get/2,
   remove/2
]).

%%%------------------------------------------------------------------
%%%
%%% Supervisor
%%%
%%%------------------------------------------------------------------
start_link(Bucket) ->
   supervisor:start_link({local, ?MODULE}, ?MODULE, []).
   
init([]) ->
   Process = {
      kvs_cache,       % child id
      {
         kvs_cache,  % Mod
         start_link, % Fun
         []          % Args
      },
      transient, 2000, worker, dynamic 
   },
   {ok,
      {
         {simple_one_for_one, 2, 1},   % 2 faults per second
         [Process]
      }
   }.

%%%------------------------------------------------------------------
%%%
%%% gen_kvs_entity
%%%
%%%------------------------------------------------------------------
construct([Bucket]) ->
   % nothing to do it is called from kvs_bucket
   {ok, self()};
construct([Bucket, Key, Item]) ->
   supervisor:start_child(?MODULE, [Bucket, Key, Item]).

config() ->
   [{supervise, supervisor}, keyspace].
   
put(Pid, Key, Item) ->
   gen_server:call(Pid, {kvs_put, Key, Item}).
   
has(Pid, Key) ->
   gen_server:call(Pid, {kvs_has, Key}).
   
get(Pid, Key) ->
   gen_server:call(Pid, {kvs_get, Key}).
   
remove(Pid, Key) ->
   gen_server:cast(Pid, {kvs_remove, Key}).
      

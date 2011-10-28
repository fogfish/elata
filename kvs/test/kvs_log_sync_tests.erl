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
-module(kvs_log_sync_tests).
-author(dmitry.kolesnikov@nokia.com).
-include_lib("eunit/include/eunit.hrl").

kvs_log_sync_test_() ->
   {
      setup,
      fun() ->
         spawn_node(dev1, fun() -> node_proc("http://localhost:8891") end)%,
         %spawn_node(dev2, fun() -> node_proc("http://localhost:8892") end),
         %spawn_node(dev3, fun() -> node_proc("http://localhost:8893") end)
      end,
      fun(_) ->
         slave:stop('dev1@localhost')%,
         %slave:stop('dev2@localhost'),
         %slave:stop('dev3@localhost')
      end,
      [
         {"Init sync", fun init_sync/0},
         {"Put sync",  {timeout, 60, fun put_sync/0}}
      ]
   }.

   
   
spawn_node(Node, Fun) ->   
   % detect path
   {file, Module} = code:is_loaded(?MODULE),
   % include all sub-projects
   Path = filename:dirname(Module) ++ "/../../*/ebin",
   {ok, _} = slave:start(localhost, Node, " -pa " ++ Path),
   spawn(
      list_to_atom(atom_to_list(Node) ++ "@localhost"),
      Fun
   ).
   
node_proc(Node) ->
   {ok, Pid} = ek:start(Node),
   ok = application:start(kvs),
   kvs_evt_sup:subscribe({kvs_evt_log, [[{ttl, 60}, {chunk, 10}]]}),
   kvs_bucket:define(kvs_evt_log, [{storage, kvs_cache_sup}, {ttlpos, 2}]),
   {ok, _} = kvs_log_sync:start_link(3),
   kvs_bucket:define(test, [{storage, kvs_sys}, event, evtlog]), 
   kvs:put(test, ek:node(), "Hello"),
   timer:sleep(60000).

   
init_sync() ->
   {ok, Pid} = ek:start("http://localhost:8890"),
   {ok,   _} = ek:connect("http://localhost:8891"),
   %{ok,   _} = ek:connect("http://localhost:8892"),
   %{ok,   _} = ek:connect("http://localhost:8893"),
   ok = application:start(kvs),
   kvs_evt_sup:subscribe({kvs_evt_log, [[{ttl, 60}, {chunk, 10}]]}),
   kvs_bucket:define(kvs_evt_log, [{storage, kvs_cache_sup}, {ttlpos, 2}]),
   {ok, _} = kvs_log_sync:start_link(5),
   kvs_bucket:define(test, [{storage, kvs_sys}]).
   
   
   
put_sync() -> 
   error_logger:info_msg('-------------------------'),
   lists:foreach(
      fun(N) -> spawn(N, fun() -> ok = kvs:put(test, ek:node(), "Hello"), error_logger:info_report([kvs:get(test, ek:node())]) end) end,
      ['dev1@localhost']
      %['dev1@localhost', 'dev2@localhost', 'dev3@localhost']
   ),
   timer:sleep(10000),
   error_logger:info_report([kvs:get(test, "http://localhost:8891")]),
   timer:sleep(60000),
   ok.   
   
   
   
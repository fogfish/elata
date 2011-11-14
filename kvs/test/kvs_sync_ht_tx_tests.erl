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
-module(kvs_sync_ht_tx_tests).
-author(dmitry.kolesnikov@nokia.com).
-include_lib("eunit/include/eunit.hrl").


kvs_sync_test_() ->
   {
      setup,
      fun() ->
         spawn_node(dev1),
         spawn(
            'dev1@localhost', 
            fun() -> 
               ok = ek:start("http://localhost:8081"),
               ok = kvs:start([cluster]),
               {ok, _} = kvs:new(test, [{storage, kvs_ets}, fed]),
               % control loop
               ek:register("/test"),
               Loop = fun(X) ->
                  receive
                     {populate, B, E, S} -> 
                        lists:foreach(fun(X) -> kvs:put(test, X, X*X) end, lists:seq(B,E,S)),
                        X(X)
                  end
               end,
               Loop(Loop)
            end
         ),
         ok = ek:start("http://localhost:8080"),
         ok = kvs:start([cluster]),
         {ok, _} = kvs:new(test, [{storage, kvs_ets}, fed]),
         ek:connect("http://localhost:8081"),
         timer:sleep(2000)
      end,
      fun(_) ->
         slave:stop('dev1@localhost')
      end,
      [
         {"P2P init sync", fun p2p_init_sync/0},
         {"P2P sync",      fun p2p_sync/0},
         {"Master-Slave sync", fun ms_sync/0},
         {"Slave-Master sync", fun sm_sync/0}
      ]
   }.

%%-------------------------------------------------------------------
%%
%% Utility functions to run distributed test
%%
%%-------------------------------------------------------------------   
spawn_node(Node) ->   
   % detect path
   {file, Module} = code:is_loaded(?MODULE),
   % include all sub-projects
   Path = filename:dirname(Module) ++ "/../../*/ebin",
   {ok, _} = slave:start(localhost, Node, " -pa " ++ Path).    
   
   
%%-------------------------------------------------------------------
%%
%% Cluster msg test
%%
%%-------------------------------------------------------------------
   
p2p_init_sync() ->
   ek:send("http://localhost:8081/test", {populate, 101, 110, 2}),
   timer:sleep(500),
   kvs_sync_ht_tx:start_link(p2p, test, "http://localhost:8081"),
   timer:sleep(2000),
   lists:foreach(
      fun(X) ->
         ?assert( {ok, X*X} =:= kvs:get(test, X) ),
         kvs:remove(test, X)
      end,
      lists:seq(101,110,2)
   ).

p2p_sync() ->
   lists:foreach(
      fun(X) -> kvs:put(test, X, X*X) end,
      lists:seq(111, 120, 1)
   ),
   kvs_sync_ht_tx:start_link(p2p, test, "http://localhost:8081"),
   timer:sleep(2000),
   lists:foreach(
      fun(X) ->
         ?assert( {ok, X*X} =:= kvs:get(test, X) ),
         kvs:remove(test, X)
      end,
      lists:seq(101,110,2)
   ),
   lists:foreach(
      fun(X) ->
         ?assert( {ok, X*X} =:= kvs:get(test, X) ),
         kvs:remove(test, X)
      end,
      lists:seq(111,120,1)
   ).
   
ms_sync() ->
   %% local test has not key
   %% sync with master mode drops remote items
   kvs_sync_ht_tx:start_link(master, test, "http://localhost:8081"),
   timer:sleep(2000),
   %%kvs_sync_ht_tx:start_link(p2p, test, "http://localhost:8081"),
   %%timer:sleep(1000),
   lists:foreach(
      fun(X) ->
         ?assert( {error, not_found} =:= kvs:get(test, X) )
      end,
      lists:seq(101,110,2)
   ),
   lists:foreach(
      fun(X) ->
         ?assert( {error, not_found} =:= kvs:get(test, X) )
      end,
      lists:seq(111,120,1)
   ).
   
sm_sync() ->
   lists:foreach(
      fun(X) -> kvs:put(test, X, X*X) end,
      lists:seq(111, 120, 1)
   ),
   kvs_sync_ht_tx:start_link(slave, test, "http://localhost:8081"),
   timer:sleep(2000),
   lists:foreach(
      fun(X) ->
         ?assert( {error, not_found} =:= kvs:get(test, X) )
      end,
      lists:seq(101,110,2)
   ),
   lists:foreach(
      fun(X) ->
         ?assert( {error, not_found} =:= kvs:get(test, X) )
      end,
      lists:seq(111,120,1)
   ).
   
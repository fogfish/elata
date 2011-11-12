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
-module(ek_tests).
-author(dmitry.kolesnikov@nokia.com).
-include_lib("eunit/include/eunit.hrl").


custer_connect_test_() ->
   {
      setup,
      fun() ->
         spawn_node(dev1),
         spawn_node(dev2),
         spawn_node(dev3),
         spawn(
            'dev1@localhost', 
            fun() -> 
               ek:start("http://localhost:8881"), 
               timer:sleep(60000)
            end
         ),
         spawn(
            'dev2@localhost', 
            fun() -> 
               ek:start("http://localhost:8882"), 
               timer:sleep(60000) 
            end
         ),
         spawn(
            'dev3@localhost', 
            fun() -> 
               ek:start("http://localhost:8883"), 
               timer:sleep(60000) 
            end
         )
      end,
      fun(_) ->
         slave:stop('dev1@localhost'),
         slave:stop('dev2@localhost'),
         slave:stop('dev3@localhost')
      end,
      [
         {"Node start",        fun node_start/0},
         {"Node join cluster", fun node_join/0},
         {"Node re-join cluster", fun node_rejoin/0}
      ]
   }.
   
%%
%%
set_node_path()  ->
   {file, Module} = code:is_loaded(?MODULE),
   Path = filename:dirname(Module) ++ "/../ebin",
   code:add_path(Path).
   
assert_nodes([H|T]) ->   
   ?assert(
      lists:member(H, ek:nodes())
   ),
   assert_nodes(T);
assert_nodes([]) ->
   ok.

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
%% Cluster connect
%%
%%-------------------------------------------------------------------
node_start() ->
   ok = ek:start("ws://localhost:8880").
   
node_join()  ->   
   {ok, _} = ek:connect("http://localhost:8881"),
   {ok, _} = ek:connect("http://localhost:8882"),
   {ok, _} = ek:connect("http://localhost:8883"),
   timer:sleep(2000),
   ?assert( lists:member("http://localhost:8881", ek:nodes()) ),
   ?assert( lists:member("http://localhost:8882", ek:nodes()) ),
   ?assert( lists:member("http://localhost:8883", ek:nodes()) ).
   
node_rejoin() ->
   erlang:exit(ek:whereis({node, "http://localhost:8881"}), error),
   ?assert( not lists:member("http://localhost:8881", ek:nodes()) ),
   ?assert( lists:member("http://localhost:8882", ek:nodes()) ),
   ?assert( lists:member("http://localhost:8883", ek:nodes()) ),
   timer:sleep(2000),
   ?assert( lists:member("http://localhost:8881", ek:nodes()) ),
   ?assert( lists:member("http://localhost:8882", ek:nodes()) ),
   ?assert( lists:member("http://localhost:8883", ek:nodes()) ).

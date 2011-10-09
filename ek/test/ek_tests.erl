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

-export([con_node1/0, con_node2/0, con_node3/0]).

custer_connect_test_() ->
   {
      setup,
      fun() ->
         {ok, _} = slave:start(localhost, dev1),
         {ok, _} = slave:start(localhost, dev2),
         {ok, _} = slave:start(localhost, dev3)
      end,
      fun(_) ->
         slave:stop('dev1@localhost'),
         slave:stop('dev2@localhost'),
         slave:stop('dev3@localhost')
      end,
      {inparallel, 
         [
            {spawn, 'dev1@localhost', [{"Node 1", fun ek_tests:con_node1/0}]},
            {spawn, 'dev2@localhost', [{"Node 2", fun ek_tests:con_node2/0}]},
            {spawn, 'dev3@localhost', [{"Node 3", fun ek_tests:con_node3/0}]}
         ]
      }
   }.

%cluster_failure_test_() ->
%   {
%      setup,
%      fun()  -> ok end,
%      {inparallel,
%         [
%         ]
%      }
%   }.
   
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
%% Cluster connect
%%
%%-------------------------------------------------------------------
con_node1() ->
   set_node_path(),
   {ok, Pid} = ek:start("ws://localhost:8880"),
   timer:sleep(1000),
   error_logger:info_msg('nodes: ~p~n', [ek:nodes()]),
   assert_nodes(["ws://localhost:8881", "ws://localhost:8882"]).
   
con_node2() ->
   set_node_path(),
   {ok, Pid} = ek:start("ws://localhost:8881"),
   ek:connect("ws://localhost:8880"),
   timer:sleep(1000),
   error_logger:info_msg('nodes: ~p~n', [ek:nodes()]),
   assert_nodes(["ws://localhost:8880", "ws://localhost:8882"]).

con_node3() ->
   set_node_path(),
   {ok, Pid} = ek:start("ws://localhost:8882"),
   ek:connect("ws://localhost:8880"),
   ek:connect("ws://localhost:8881"),
   timer:sleep(1000),
   error_logger:info_msg('nodes: ~p~n', [ek:nodes()]),
   assert_nodes(["ws://localhost:8880", "ws://localhost:8881"]).

%%-------------------------------------------------------------------
%%
%% Cluster node failure
%%
%%-------------------------------------------------------------------
   
   
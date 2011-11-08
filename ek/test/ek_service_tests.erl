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
-module(ek_service_tests).
-author(dmitry.kolesnikov@nokia.com).
-include_lib("eunit/include/eunit.hrl").

-export([input/1, output/1]).

%%%------------------------------------------------------------------
%%%
%%% 
%%%
%%%------------------------------------------------------------------
input({msg, X}) ->
   ek_service:send("http://localhost:8900/test", {msg, X + X}).
   
output({msg, X}) ->
   {msg, X + X}.

   
cluster_service_test_() ->
   {
      setup,
      fun() ->
         ets:new(test, [named_table, public]),
         spawn_node(dev1),
         spawn(
            'dev1@localhost', 
            fun() -> 
               ek:start("http://localhost:8901"),
               ek_service:start_link("/test", ?MODULE),
               timer:sleep(60000)
            end
         )
      end,
      fun(_) ->
         slave:stop('dev1@localhost')
      end,
      [
         {"Service start",  fun node_start/0},
         {"Service msg",    fun node_msg/0}
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
node_start() ->
   ek:start("http://localhost:8900"),
   ek:connect("http://localhost:8901"),
   spawn(
      fun() -> 
         ek:listen("/test"),
         Loop = fun(X) ->
            receive
               Msg ->
                  ets:insert(test, Msg),
                  X(X)
            end
         end,
         Loop(Loop)
      end
   ),
   timer:sleep(2000).   
   
   
node_msg()  ->   
   ek:send("http://localhost:8901/test", {msg, 1}),
   timer:sleep(500),
   %R = ets:lookup(test, msg),
   %error_logger:info_report([{r, R}]),
   ?assert(
      [{msg, 4}] =:= ets:lookup(test, msg)
   ).   
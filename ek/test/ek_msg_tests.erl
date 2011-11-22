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
-module(ek_msg_tests).
-author(dmitry.kolesnikov@nokia.com).
-include_lib("eunit/include/eunit.hrl").

-define(CHARS, "qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM1234567890").

custer_msg_test_() ->
   {
      setup,
      fun() ->
         ets:new(test, [named_table, public]),
         spawn_node(dev1),
         spawn(
            'dev1@localhost', 
            fun() -> 
               ek:start("node://localhost:8891"), 
               spawn(
                  fun() -> 
                     ek:register("urn:/test"), 
                     Loop = fun(X) -> 
                        receive 
                           Msg -> 
                              ek:send("urn://localhost:8890/test", Msg), 
                              X(X)
                        end
                     end,
                     Loop(Loop)
                  end
               ),
               timer:sleep(60000)
            end
         )
      end,
      fun(_) ->
         slave:stop('dev1@localhost')
      end,
      [
         {"Node start",  fun node_start/0},
         {"Node short msg",  fun node_short_msg/0},
         {"Node short lst",  fun node_short_lst/0},
         {"Node mid msg",  fun node_mid_msg/0},
         {"Node mid lst",  fun node_mid_lst/0},
         {"Node large msg",  fun node_large_msg/0},
         {"Node large lst",  {timeout, 10, fun node_large_lst/0}}
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
   ek:start("node://localhost:8890"),
   ek:connect("node://localhost:8891"),
   spawn(
      fun() -> 
         ek:register("urn:/test"),
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
   
node_short_msg()  ->   
   Msg = {1, "value"},
   ek:send("urn://localhost:8891/test", Msg),
   timer:sleep(500),
   ?assert(
      [Msg] =:= ets:lookup(test, 1)
   ).

node_short_lst() ->
   Seq = lists:seq(1,10),
   Msg = lists:map(fun(X) -> {X, "value"} end, Seq),
   ek:send("urn://localhost:8891/test", Msg),
   timer:sleep(500),
   lists:foreach(
      fun(X) ->
         ?assert(
            [{X, "value"}] =:= ets:lookup(test, X)
         )
      end,
      Seq
   ).
   
node_mid_msg() ->
   Msg = {1, rnd_string(1024, ?CHARS)},
   ek:send("urn://localhost:8891/test", Msg),
   timer:sleep(500),
   R = ets:lookup(test, key1),
   %error_logger:info_report([{r, R}]).
   ?assert(
      [Msg] =:= ets:lookup(test, 1)
   ).
   
node_mid_lst() ->
   Val = rnd_string(1024, ?CHARS),
   Seq = lists:seq(1,100),
   Msg = lists:map(fun(X) -> {X, Val} end, Seq),
   ek:send("urn://localhost:8891/test", Msg),
   timer:sleep(500),
   lists:foreach(
      fun(X) ->
         ?assert(
            [{X, Val}] =:= ets:lookup(test, X)
         )
      end,
      Seq
   ).   
   
   
node_large_msg() ->
   Msg = {1, rnd_string(102400, ?CHARS)},
   ek:send("urn://localhost:8891/test", Msg),
   timer:sleep(500),
   R = ets:lookup(test, key1),
   %error_logger:info_report([{r, R}]).
   ?assert(
      [Msg] =:= ets:lookup(test, 1)
   ).
   
node_large_lst() ->
   Val = rnd_string(102400, ?CHARS),
   Seq = lists:seq(1,100),
   Msg = lists:map(fun(X) -> {X, Val} end, Seq),
   ek:send("urn://localhost:8891/test", Msg),
   timer:sleep(9000),
   lists:foreach(
      fun(X) ->
         ?assert(
            [{X, Val}] =:= ets:lookup(test, X)
         )
      end,
      Seq
   ).      
   
%% from http://blog.teemu.im/2009/11/07/generating-random-strings-in-erlang/
rnd_string(Length, AllowedChars) ->
   lists:foldl(
      fun(_, Acc) ->
         [lists:nth(
            random:uniform(length(AllowedChars)),
            AllowedChars
         )] ++ Acc
      end, 
      [], 
      lists:seq(1, Length)
   ).

   
   % {ok, _} = ek:connect("http://localhost:8891"),
   % {ok, _} = ek:connect("http://localhost:8892"),
   % {ok, _} = ek:connect("http://localhost:8893"),
   % timer:sleep(2000),
   % ?assert( lists:member("http://localhost:8891", ek:nodes()) ),
   % ?assert( lists:member("http://localhost:8892", ek:nodes()) ),
   % ?assert( lists:member("http://localhost:8893", ek:nodes()) ).
   
   
% node_rejoin() ->
   % {ok, Pid} = ek:connect("http://localhost:8891"),
   % erlang:exit(Pid, error),
   % ?assert( not lists:member("http://localhost:8891", ek:nodes()) ),
   % ?assert( lists:member("http://localhost:8892", ek:nodes()) ),
   % ?assert( lists:member("http://localhost:8893", ek:nodes()) ),
   % timer:sleep(2000),
   % ?assert( lists:member("http://localhost:8891", ek:nodes()) ),
   % ?assert( lists:member("http://localhost:8892", ek:nodes()) ),
   % ?assert( lists:member("http://localhost:8893", ek:nodes()) ).
   % 
   

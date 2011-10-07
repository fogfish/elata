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
-module(ek_echo).
-author(dmitry.kolesnikov@nokia.com).

%%
%% Example echo cluster
%%

-export([
   join/1,
   join/2,
   ping/0
]).

join(Node) ->
   {ok, Pid} = ek:start(Node),
   spawn(
      fun() ->
         ek:subscribe("/echo"),
         loop()
      end
   ).
   
join(Node, Peer) ->
   {ok, Pid} = ek:start(Node),
   ek:connect(Peer),
   spawn(
      fun() ->
         ek:subscribe("/echo"),
         loop()
      end
   ).
   
ping() ->
   ping(0).
ping(TTL) ->
   Nodes = ek:nodes(),   
   Index = random:uniform(length(Nodes)), 
   Node  = lists:nth(Index, Nodes),
   ek:send(Node ++ "/echo", {ping, ek:node(), TTL}).

   
loop() ->
   receive
      {ping, From, TTL} ->
         io:format('ping from ~p (ttl=~p)~n', [From, TTL]),
         timer:sleep(1000),
         ping(TTL + 1),
         loop();
      Any ->
         loop()
   end.
      
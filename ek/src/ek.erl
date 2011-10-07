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
-module(ek).
-author(dmitry.kolesnikov@nokia.com).

%%
%% ek is Erlang Cluster, alternative method to build 
%% globally (geo) distributed Erlang clusters
%%

-export([
   start/1,
   nodes/0,
   connect/1,
   monitor/1,
   send/2
]).


%%
%% starts Erl Cluster node
start(Node) ->
   ek_app:start(permanent, [{node, Node}]).

%%
%% list of connected nodes
nodes() ->
   [N || {N,_} <- ets:tab2list(ek_nodes), is_list(N)].  
   
%%
%% connect to node
connect(Node) ->
   ek_ws_sup:connect(Node).

%%
%% monitor a node
monitor(Node) ->
   case ets:lookup(ek_nodes, Node) of
      [{Node, Pid}] ->
         erlang:monitor(process, Pid);
      _             ->
         {error, not_found}
   end.

   
%%
%% send a message to node
send(Uri, Msg) ->
   U    = ek_uri:new(Uri),
   Node = atom_to_list(proplists:get_value(schema, U)) ++ "://" ++ 
          binary_to_list(proplists:get_value(host, U)) ++ ":" ++
          integer_to_list(proplists:get_value(port, U)),
   case ets:lookup(ek_nodes, Node) of
      [{Node, Pid}] ->
         gen_fsm:send_event(Pid, {msg, Uri, Msg});
      _             ->
         {error, not_found}
   end.
   
   
   
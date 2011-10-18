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
-module(ek).
-author(dmitry.kolesnikov@nokia.com).
-include("include/ek.hrl").

%%
%% Erlang Cluster (ek) is an overlay to build a geo distributed clusters
%% (alternative method to Erlang build in facilities)
%%


-export([
   start/1,
   start/2,
   % node management
   node/0,
   nodes/0,
   connect/1,
   disconnect/1,
   monitor/0,
   monitor/1,
   demonitor/0,
   demonitor/1,
   % messaging
   send/2,
   multicast/2,
   broadcast/2,
   listen/1
]).


%%
%% starts cluster management application
%% Node - list(), local node identity, uri (e.g. node://168.192.0.1:8080).
start(Node) ->
   start(Node, []).
start(Node, Config) ->
   {ok, _} = case erlang:whereis(ek_sup) of
      undefined -> ek_app:start(permanent, Config);
      Pid       -> {ok, Pid}
   end,
   case ek:node() of
      undefined -> {ok, _} = ek_sup:listen([{node, Node}]);
      Uri       -> {error, {already_exists, Uri}}
   end.
           
%%-------------------------------------------------------------------
%%
%% Node Management
%%
%%-------------------------------------------------------------------
   
%%
%% Name of itself
node() ->
    case ets:match_object(ek_nodes, {ek_node, '_', self}) of
       []     -> undefined;
       [Node] -> Node#ek_node.uri;
       List   -> lists:map(fun(N) -> N#ek_node.uri end, List)
    end.
   
%%
%% Retrive list of connected nodes
nodes() ->
   SF = fun(X) ->
      {ok, Info} = ek_prot:node_info(X#ek_node.pid),
      case proplists:get_value(state, Info) of
         'CONNECTED' -> true;
         _           -> false
      end
   end,
   [
      N#ek_node.uri || N <- ets:match_object(ek_nodes, '_'), 
                       N#ek_node.pid =/= self, SF(N) =:= true
   ].  
   
%%
%% Initiates a connection with remote node
%% connect(Node) -> {ok, Pid} | {error, ...}
%%    Node - list(), remote node identity
connect(Node) ->
   case ets:lookup(ek_nodes, Node) of
      [Node] -> {ok, Node#ek_node.pid};
      _      -> ek_prot_sup:create(Node)
   end.

%%
%% Forces to disconnect node
disconnect(Node) ->
   {error, not_implemented}.

%%
%% monitor/demonitor cluster events
monitor() ->
   ek_evt_srv:subscribe(self()).
monitor(EvtHandler) ->
   ek_evt:subscribe(EvtHandler).

demonitor() ->
   ek_evt_srv:unsubscribe(self()).
demonitor(EvtHandler) ->
   ek_evt:unsubscribe(EvtHandler).
   
%%
%% send a message to uri (node + ep)
send(Uri, Msg) ->
   U    = ek_uri:new(Uri),
   Node = atom_to_list(proplists:get_value(schema, U)) ++ "://" ++ 
          binary_to_list(proplists:get_value(host, U)) ++ ":" ++
          integer_to_list(proplists:get_value(port, U)),
   case ets:lookup(ek_nodes, Node) of
      [N] ->
         ek_prot:send(N#ek_node.pid, Uri, Msg);
       _             ->
         {error, node_not_found}
   end.

%%
%% send a message to multiple uri
multicast(Uris, Msg) ->
   lists:foreach(fun(U) -> send(U, Msg) end, Uris).

%%
%% send a message to ep of connected nodes
broadcast(EP, Msg) ->
   lists:foreach(
      fun(N) ->
         Uri = N ++ EP,
         send(Uri, Msg)
      end,
      ek:nodes()
   ).
   
   
   
%%
%% subscribe
listen({drop, Path}) ->
   ets:delete_object(ek_dispatch, {list_to_binary(Path), self()});
   
listen(Path) ->
   ets:insert(ek_dispatch, {list_to_binary(Path), self()}).


   
   
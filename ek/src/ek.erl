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
%% The module is a facade to various sub-processes
%%

-export([
   start/1,
   start/2,
   % local process management
   register/1,
   register/2,
   unregister/1,
   whereis/1,
   registered/0,
   registered/1,
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
   multicast/3,
   broadcast/2
]).


%%
%% starts cluster management application
%% Node - list(), local node identity, uri (e.g. node://168.192.0.1:8080).
start(Node) ->
   start(Node, []).
start(Node, Config) ->
   lists:foreach(
      fun({K, V}) -> application:set_env(ek, K, V) end,
      [{node, Node} | Config]
   ),
   {file, Module} = code:is_loaded(?MODULE),
   AppFile = filename:dirname(Module) ++ "/" ++ atom_to_list(?MODULE) ++ ".app",
   {ok, [{application, _, List}]} = file:consult(AppFile), 
   Apps = proplists:get_value(applications, List, []),
   lists:foreach(
      fun(X) -> application:start(X) end,
      Apps
   ),
   application:start(?MODULE).

%%-------------------------------------------------------------------
%%
%% local Process Management (see src/ek_reg.erl)
%%
%%-------------------------------------------------------------------
register(Uid) ->
   ek_reg:register(Uid, self()).

register(Uid, Pid) -> 
   ek_reg:register(Uid, Pid).

unregister(Uid) ->
   ek_reg:unregister(Uid).
   
whereis(Uid) ->
   ek_reg:whereis(Uid).
   
registered() ->
   ek_reg:registered().
   
registered(Grp) ->
   ek_reg:registered(Grp).
           
%%-------------------------------------------------------------------
%%
%% Node Management
%%
%%-------------------------------------------------------------------
   
%%
%% node() -> []
node() ->
   case ek:registered(listen) of
      []   -> throw(no_ek);
      List -> lists:map(fun({listen, Uri}) -> Uri end, List)
   end.
   
%%
%% Retrive list of connected nodes
nodes() ->
   lists:map(
      fun({node, Uri}) -> Uri end,
      ek:registered(node)
   ).
   
%%
%% Initiates a connection with remote node
%% connect(Node) -> {ok, Pid} | {error, ...}
%%    Node - list(), remote node identity
connect(Node) ->
   ek_prot_sup:create(Node).

%%
%% Forces to disconnect node
disconnect(_Node) ->
   throw(not_implemented).

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

%%%------------------------------------------------------------------
%%%
%%% Messaging
%%%
%%%------------------------------------------------------------------

%%
%% send(Uid, Msg) -> ok
%%   Uid = list() | tuple()
%%
%% sends message to registered process, 
%% if process do not exists locally and Uid is Uri then Msg is routed to remote node
%% Failure: {badarg, {Uid, Msg}} if process and/or node cannot be found
send(Uid, Msg) when is_tuple(Uid) -> 
   case ek:whereis(Uid) of
      undefined -> throw({badarg, {Uid, Msg}});
      Pid       -> Pid ! Msg
   end,
   ok;

send(Uri, Msg) ->
   case ek:whereis(Uri) of
      undefined ->
         Host = ek_uri:host(Uri),
         case ek:whereis({node, Host}) of
            undefined -> throw({badarg, {Uri, Msg}});
            Pid       -> ek_prot:send(Pid, Uri, Msg)
         end;   
      Pid       -> Pid ! Msg
   end,
   ok.


%%
%% multicast(Uids, Msg) -> ok
%%   Uids = list()
%%
%% Multicast mesage to multiple processes
%% Failure: {badarg, {Uid, Msg}} if one of process cannot be found
multicast(Uris, Msg) ->
   lists:foreach(fun(U) -> send(U, Msg) end, Uris).

multicast(Nodes, Path, Msg) ->
   lists:foreach(
      fun(Node) ->
         Uri = Node ++ Path,
         ek:send(Uri, Msg)
      end,
      Nodes
   ).   
   
%%
%% broadcast(Path, Msg) -> ok
%%
%% Broadcast message to all connected remote nodes
broadcast(Path, Msg) ->
   lists:foreach(
      fun({node, Node}) ->
         Uri = Node ++ Path,
         ek:send(Uri, Msg)
      end,
      ek:registered(node)
   ).
      
   
% %%%------------------------------------------------------------------
% %%%
% %%% Private
% %%%
% %%%------------------------------------------------------------------
% 
% %%
% %% list of connected nodes and they pids
% connected_nodes() ->
   % SF = fun(X) ->
      % {ok, Info} = ek_prot:node_info(X#ek_node.pid),
      % case proplists:get_value(state, Info) of
         % 'CONNECTED' -> true;
         % _           -> false
      % end
   % end,
   % [
      % N || N <- ets:match_object(ek_nodes, '_'), 
           % N#ek_node.pid =/= self, 
           % is_process_alive(N#ek_node.pid), 
           % SF(N) =:= true
   % ].
   % 
% %%
% %% list of alive nodes and they pids   
% alive_nodes() ->
   % [
      % N || N <- ets:match_object(ek_nodes, '_'), 
           % N#ek_node.pid =/= self, 
           % is_process_alive(N#ek_node.pid)
   % ].
% 
% %%
% %% check node from Uri
% check_node(U) ->
   % case proplists:get_value(host, U) of
      % undefined -> true;
      % _         ->
         % Node = atom_to_list(proplists:get_value(schema, U)) ++ "://" ++ 
                % binary_to_list(proplists:get_value(host, U)) ++ ":" ++
                % integer_to_list(proplists:get_value(port, U)),
         % lists:keyfind(Node, 2, alive_nodes())
   % end.
   % 
   % 
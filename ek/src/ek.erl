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

register(Uri) ->
   ek_reg:register(Uri, self()).

register(Uri, Pid) -> 
   ek_reg:register(Uri, Pid).

unregister(Uri) ->
   ek_reg:unregister(Uri).
   
whereis(Uri) ->
   ek_reg:whereis(Uri).
   
registered() ->
   ek_reg:registered().
   
registered(Schema) ->
   ek_reg:registered(Schema).
           
%%-------------------------------------------------------------------
%%
%% Node Management
%%
%%-------------------------------------------------------------------
   
%%
%% node() -> []
%%
%% local node name: 'ek-listen:' process
node() ->
   List = ek_reg:q(
      fun(ek) -> true;  (_) -> false end,
      fun(undefined)  -> false; (_) -> true end,
      fun(_) -> true end
   ),
   case List of
      []         -> throw(no_ek);
      [Node | _] -> ek_uri:authority(Node)
   end.
   
%%
%% nodes() ->
%% Retrive list of connected nodes
nodes() ->
   ek_reg:q(
      fun(ek)         -> false; (_) -> true  end,
      fun(undefined)  -> false; (_) -> true  end,
      fun(<<"/">>)    -> true;  (_) -> false end
   ).
   
%%
%% Initiates a connection with remote node
%% connect(Node) -> {ok, Pid} | {error, ...}
%%    Node - list(), remote node identity
connect({node, Node, <<"/">>} = Uri) ->
   ek_prot_sup:create(Uri);
connect(Uri) ->
   connect(ek_uri:new(Uri)).
   
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
%% send(Uri, Msg) -> ok
%%   Uri = list() | tuple()
%%
%% Sends message to process identified by URI
%% if process do not exists locally then Msg is routed to remote node
%% Failure: {badarg, Uri} if process or node cannot be found
send({_Scheme, undefined, _Path} = Uri, Msg) ->
   case ek:whereis(Uri) of
      undefined -> throw({badarg, Uri});
      Pid       -> Pid ! Msg, ok
   end;
send({Scheme, Auth, Path} = Uri, Msg) ->
   case ek:node() of
      Auth -> 
         % Auth = ek:node() message for this node
         send({Scheme, undefined, Path}, Msg);
      _    ->
         case ek:whereis({node, Auth, <<"/">>}) of
            undefined -> throw({badarg, Uri});
            Pid       -> ek_prot:send(Pid, Uri, Msg)
         end
   end;
send(Uri, Msg) ->
   send(ek_uri:new(Uri), Msg).


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
      
   
%%%------------------------------------------------------------------
%%%
%%% Private
%%%
%%%------------------------------------------------------------------

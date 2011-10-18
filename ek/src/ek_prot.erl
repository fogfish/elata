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
-module(ek_prot).
-behaviour(gen_fsm).
-author(dmitry.kolesnikov@nokia.com).
-include("include/ek.hrl").

%%
%% ek node management protocol
%%
%% TODO:
%%   - message queue
%%   - packet transmission over stream
%%   - disable code injection (trusted nodes only)

-export([
   %% api
   start_link/2,
   node_info/1,
   send/3,
   %% gen_fsm
   init/1, 
   'IDLE'/2,
   'CONNECTED'/2,
   handle_event/3,
   handle_sync_event/4, 
   handle_info/3, 
   terminate/3, 
   code_change/4                                         
   %% api
   %send/2,
   %multicast/2,
   %recv/1
]).

-record(fsm, {
   node,      % node name
   sock,      % node transport
   attempt,   % number of connection attempts
   q          % message queue
}).

-define(T_NODE_HEARTBEAT, 30000). %% heartbeat timer for node


%%
%% debug macro
-ifdef(DEBUG).
-define(DEBUG(M), error_logger:info_report([{?MODULE, self()}] ++ M)).
-else.
-define(DEBUG(M), true).
-endif.

%%%------------------------------------------------------------------   
%%%
%%% API
%%%
%%%------------------------------------------------------------------

%%
%%
node_info(Pid) ->
   gen_fsm:sync_send_all_state_event(Pid, get_node_info).
   
%%
%%
send(Pid, Uri, Msg) ->
   gen_fsm:send_all_state_event(Pid, {send, Uri, Msg}).
   
%%%------------------------------------------------------------------   
%%%
%%% gen_server
%%%
%%%------------------------------------------------------------------
start_link(Config, Node) ->
   gen_fsm:start_link(?MODULE, [Config, Node], []).
   
init([Config, Node]) ->
   case is_node(Node) of
      true  ->
         % node process exists 
         {stop, normal};
      false ->
         ?DEBUG([{node, Node}, {pid, self()}]),
         gen_fsm:send_event_after(1000, node_connect),
         {ok,
            'IDLE',
            #fsm{
               node    = Node,
               sock    = undefined,
               attempt = 0
            }
         }
   end.
   
%%
%%   
'IDLE'(node_connect, State) ->
   % connect to node
   ?DEBUG([{connecting, State#fsm.node}]),
   ek_ws_sup:create({connect, State#fsm.node, self()}),
   gen_fsm:send_event_after(?T_NODE_HEARTBEAT, node_no_connect),
   {next_state, 'IDLE', State};
   
'IDLE'(node_no_connect, State) ->  
   ?DEBUG([{timeout, State#fsm.node}]),
   gen_fsm:send_event_after(
      erlang:round(
         math:pow(2, State#fsm.attempt rem 16) * 1000 %% max 18 hour to wait
      ),
      node_connect
   ),
   {next_state, 'IDLE', State#fsm{attempt = State#fsm.attempt + 1}};
   
'IDLE'({socket, Sock}, State) ->
   join_node(State#fsm.node),
   %lists:foreach(
   %   fun(X) -> sock_send(Sock, X) end,
   %   State#fsm.q
   %),
   {next_state, 'CONNECTED', State#fsm{q= [], sock = Sock, attempt = 0}};
   
'IDLE'({send, Uri, Msg}, State) ->
   {next_state, 'IDLE', State#fsm{q = State#fsm.q ++ [{msg, Uri, Msg}]}};
   
'IDLE'(_Evt, State)  ->
   {next_state, 'IDLE', State}.  
  

%%
%%   
'CONNECTED'(heartbeat, State) ->
   % heartbeat timer is expired
   gen_fsm:send_event_after(?T_NODE_HEARTBEAT, heartbeat),
   sock_send(State#fsm.sock, {node, ek:node()}),
   {next_state, 'CONNECTED', State};
'CONNECTED'({node, _Node}, State) ->
   % heartbeat message is recieved
   {next_state, 'CONNECTED', State};
   
'CONNECTED'({send, Uri, Msg}, State) ->
   % transmit message
   sock_send(State#fsm.sock, {msg, Uri, Msg}),
   {next_state, 'CONNECTED', State};
'CONNECTED'({msg, Uri, Msg}, State) ->
   % receive message % dispath it 
   U = ek_uri:new(Uri),
   case ets:lookup(ek_dispatch, proplists:get_value(path, U)) of
      []    -> ok;
      List  ->
         [ Pid ! Msg || {_, Pid} <- List],
         ok
   end,   
   {next_state, 'CONNECTED', State};
   
'CONNECTED'({tcp, _Sock, Pckt}, State) ->
   %% TODO: prevent code injection
   Msg = binary_to_term(Pckt), 
   ek_prot:'CONNECTED'(Msg, State);
   
'CONNECTED'({tcp_closed, _Sock}, State) ->
   leave_node(State#fsm.node),
   {next_state, 'IDLE', State};

'CONNECTED'({tcp_error, _Sock, Reason}, State) ->
   leave_node(State#fsm.node),
   {next_state, 'IDLE', State};
   
'CONNECTED'(Evt, State) ->
   ?DEBUG([{state, 'CONNECTED'}, {evt, Evt}]),
   {next_state, 'CONNECTED', State}.

   
handle_event(Msg, Name, State) ->
   erlang:apply(?MODULE, Name, [Msg, State]).
   
handle_info(Msg, Name, State) ->
   erlang:apply(?MODULE, Name, [Msg, State]).

terminate(Reason, Name, State) ->
   ok.
   
code_change(_OldVsn, Name, State, _Extra) ->
   {ok, Name, State}.   

%%
%% Sync API Impl
%%
handle_sync_event(get_node_info, _From, Name, State) ->   
   R = [
      {state, Name}
   ],
   {reply, {ok, R}, Name, State};
   
handle_sync_event(_Req, _From, Name, State) ->
   {reply, {error, not_supported}, Name, State}.   
   
   
%%%------------------------------------------------------------------
%%%
%%% Private
%%%
%%%------------------------------------------------------------------

%%%
%%% Node registry
%%%

%%
%% checks if Node exists and alive
is_node(Node) ->
   case ets:match_object(ek_nodes, {ek_node, Node, '_'}) of
      []  -> 
         ets:insert(ek_nodes, #ek_node{uri=Node, pid=self()}),
         false;
      [N] -> 
         if
            N#ek_node.pid =/= self() -> 
               case erlang:is_process_alive(N#ek_node.pid) of
                  true  -> 
                     true;
                  false -> 
                     % node exists but process is not alive
                     % overtake it
                     ets:insert(ek_nodes, #ek_node{uri=Node, pid=self()}),
                     false
               end;
            N#ek_node.pid =:= self() -> 
               ets:insert(ek_nodes, #ek_node{uri=Node, pid=self()}),
               false
         end
   end. 

%%
%% register node
join_node(Node)  ->
   gen_fsm:send_event_after(?T_NODE_HEARTBEAT, heartbeat),
   ek_evt:join(Node),
   ?DEBUG([{join, Node}]). 
   
%%
%% remove node
leave_node(Node) ->  
   gen_fsm:send_event_after(?T_NODE_HEARTBEAT, node_connect),
   ek_evt:leave(Node),
   ?DEBUG([{leave, Node}]).
   %case ets:match_object(ek_nodes, {ek_node, Node, '_'}) of
   %   []  ->
   %      ok;
   %   [N] ->
   %      ets:delete(ek_nodes, Node),
   %      ek_evt:leave(Node),
   %      ?DEBUG([{leave, Node}, {pid, N#ek_node.pid}])
   %end.   
   
   
sock_send(Sock, Msg) ->
   Pckt = term_to_binary(Msg),
   ok = gen_tcp:send(Sock, Pckt),
   ?DEBUG([{send, Msg}]).   
   

   
   

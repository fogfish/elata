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
%%   - node alive timeout
%%   - disable code injection (trusted nodes only)
%%   - ttl for queued messages
                                  
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
]).

-record(fsm, {
   node,    % node identity (uri)
   name,    % node name (human readable name)
   sock,    % node transport
   attempt, % number of connection attempts
   p,       % inbound fragmented packet
   q        % outbound message queue
}).

-define(T_NODE_HEARTBEAT, 30000). %% heartbeat timer for node
-define(T_MSG_SEND,           5). %% message send tick (change one packet delimiter is implemented)


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
%%% gen_fsm
%%%
%%%------------------------------------------------------------------
start_link(Config, Uri) ->
   gen_fsm:start_link(?MODULE, [Config, Uri], []).
   
init([_Config, Nid]) ->
   Uri = ek_uri:set(q, [], Nid),
   case ek:whereis(Uri) of
      undefined ->
         ?DEBUG([{node, Uri}, {pid, self()}]),
         ek:register(Uri, self()),
         gen_fsm:send_event_after(1000, node_connect),
         {ok,
            'IDLE',
            #fsm{
               node    = Uri,
               name    = ek_uri:get(q, Nid),
               sock    = undefined,
               attempt = 0,
               q       = queue:new()
            }
         };
      _         ->
         % process exists
         {stop, normal}
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
   
'IDLE'({send, Uri, Msg}, S) ->   
   % TODO: for idle node message q is disabled (fix via state machine)
   % {next_state, 'IDLE', S#fsm{q = q_in(Uri, Msg, S#fsm.q)}}; 
   {next_state, 'IDLE', S}; 

'IDLE'({socket, Sock}, S) ->
   join_node(S#fsm.node),
   {next_state, 
      'CONNECTED', 
      S#fsm{
         sock = Sock,
         attempt = 0
      },
      0   %% timeout at connected state is used to schedule q-flush 
   };
   
'IDLE'(_Evt, State)  ->
   {next_state, 'IDLE', State}.  
  

%%
%%
'CONNECTED'(heartbeat, State) ->
   % heartbeat timer is expired
   gen_fsm:send_event_after(?T_NODE_HEARTBEAT, heartbeat),
   %sock_send(State#fsm.sock, {node, ek:node()}),
   {next_state, 'CONNECTED', State, ?T_MSG_SEND};
'CONNECTED'({node, _Node}, State) ->
   % heartbeat message is recieved
   {next_state, 'CONNECTED', State, ?T_MSG_SEND};

%%
%% flush message queue   
'CONNECTED'(timeout, S) ->
   ek_prot:'CONNECTED'(qflush, S);

'CONNECTED'({send, Uri, Msg}, S) ->   
   ek_prot:'CONNECTED'(qflush, S#fsm{q = q_in(Uri, Msg, S#fsm.q)});   
   
'CONNECTED'(qflush, S) ->
   NQ = q_out(S#fsm.sock, S#fsm.q), 
   case queue:is_empty(NQ) of
      true  -> ok;
      false -> gen_fsm:send_event_after(0, qflush)
   end,
   {next_state, 'CONNECTED', S#fsm{q = NQ}};
   
%%
%% receive message
'CONNECTED'({tcp, Sock, <<131, 109, Len:32/big, Data/binary>>}, S) ->   
   % head of message
   % ?DEBUG([{len, Len}, {size, size(Data)}, {data, Data}]),
   if 
      Len =:= size(Data) -> 
         dispatch(Data),
         {next_state, 'CONNECTED', S};
      Len <   size(Data) ->
         <<Pckt:Len/binary, Rest/binary>> = Data,
         dispatch(Pckt),
         ek_prot:'CONNECTED'({tcp, Sock, Rest}, S);
      Len >   size(Data) ->
         {next_state, 'CONNECTED', S#fsm{p = {Len, Data}}}
   end;
'CONNECTED'({tcp, Sock, Tail}, #fsm{p = {Len, Head}} = S) ->
   % message continuation 
   Data = <<Head/binary, Tail/binary>>,
   % ?DEBUG([{len, Len}, {size, size(Data)}, {data, Data}]),
   if
      Len =:= size(Data) -> 
         dispatch(Data),
         {next_state, 'CONNECTED', S};
      Len <   size(Data) ->
         <<Pckt:Len/binary, Rest/binary>> = Data,
         dispatch(Pckt),
         ek_prot:'CONNECTED'({tcp, Sock, Rest}, S);
      Len >   size(Data) ->
         {next_state, 'CONNECTED', S#fsm{p = {Len, Data}}}
   end;
'CONNECTED'({tcp_closed, _Sock}, State) ->
   leave_node(State#fsm.node),
   {next_state, 'IDLE', State};

'CONNECTED'({tcp_error, _Sock, Reason}, State) ->
   leave_node(State#fsm.node),
   {next_state, 'IDLE', State};
   
'CONNECTED'(Evt, State) ->
   ?DEBUG([{state, 'CONNECTED'}, {evt, Evt}]),
   {next_state, 'CONNECTED', State}.

   
%%
%%
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
handle_sync_event(get_node_info, _From, Name, S) ->   
   R = [
      {node,  S#fsm.node},
      {name,  S#fsm.name},
      {state, Name}
   ],
   {reply, {ok, R}, Name, S};
   
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
%% register node
join_node(Node)  ->
   %%ek:register(Node),
   gen_fsm:send_event_after(?T_NODE_HEARTBEAT, heartbeat),
   ek_evt:join(Node),
   ?DEBUG([{join, Node}]). 
   
%%
%% remove node
leave_node(Node) ->  
   %%ek:unregister(Node),
   gen_fsm:send_event_after(?T_NODE_HEARTBEAT, node_connect),
   ek_evt:leave(Node),
   ?DEBUG([{leave, Node}]).
   

%%
%% queue message(s)
%% q(Uri, Msg, Q) -> Q 
%%   Uri - binary() destination end-point
%%   Msg - tuple()  message
q_in(Uri, Msg, Q) when is_list(Msg) ->
   lists:foldl(fun(M, A) -> q_in(Uri, M, A) end, Q, Msg);

q_in(Uri, Msg, Q) ->
   ?DEBUG([{send, Msg}]),
   Pckt = term_to_binary({ek_msg, Uri, Msg}),
   %% double encoding is used to packetize   
   queue:in(term_to_binary(Pckt), Q).

%%
%% transmit first message from queue
q_out(Sock, Q) ->
   case queue:out(Q) of
      {{value, Pckt}, NQ} ->
         ok = gen_tcp:send(Sock, Pckt),
         NQ;
      {empty,         NQ} ->
         NQ
   end.
   
%%
%% dispatch received packet
dispatch(Pckt) ->
   {ek_msg, Uri, Msg} = binary_to_term(Pckt),
   ?DEBUG([{uri, Uri}, {recv, Msg}]),
   ek:send(Uri, Msg).      
   

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
-module(ek_ws).
-behaviour(gen_fsm).
-author(dmitry.kolesnikov@nokia.com).

%%
%% WebSocket transport for Erlang Cluster
%% fsm:
%%   - LISTEN:     Listens incoming connection (passive)
%%   - CONNECTING: Tries to connect (active)
%%   - HANDSHAKE:  WS Connection setup
%%   - CONNECTED:

-export([
   start_link/2,
   %% gen_fsm
   init/1,
   'LISTEN'/2,
   'CONNECTING'/2,
   'HANDSHAKE'/2,
   'CONNECTED'/2,
   'HANDOVER'/2,
   handle_event/3,
   handle_sync_event/4,
   handle_info/3,
   terminate/3,
   code_change/4
]).

-record(fsm, {
   passive,  %%  
   node, 
   config,
   recon,   %% ref to node re-connect timer
   sock
}).
%%-define(SOCK_OPTS, [{active, true}, binary, {packet, 0}]).
-define(SOCK_OPTS, [{active, true}, {mode, binary}]).

-define(T_TCP_SOCK_CON,  20000).  %% timer for tcp socket connection
-define(T_WEB_SOCK_CON,  20000).  %% timer for web socket connection
-define(T_PEER_RECON,    60000).  %% timer to node reconnect

%%
%%
start_link(Config, {listen, Port}) ->
   gen_fsm:start_link(?MODULE, [Config, {listen, Port}], []);
start_link(Config, {accept, Sock}) ->
   gen_fsm:start_link(?MODULE, [Config, {accept, Sock}], []);
start_link(Config, {connect, Node}) ->
   gen_fsm:start_link(?MODULE, [Config, {connect, Node}], []).
   
init([Config, {listen, Port}]) ->
   {ok, Sock} = gen_tcp:listen(Port, ?SOCK_OPTS),
   {ok, 
      'LISTEN', 
      #fsm{
         passive = true, 
         node    = undefined,
         config  = Config, 
         recon   = undefined,
         sock    = Sock
      }, 
      0   %% timeout jumps to accept
   };
   
init([Config, {accept, Sock}]) ->
   {ok, 
      'LISTEN', 
      #fsm{
         passive = true, 
         node    = undefined,
         config  = Config, 
         recon   = undefined,
         sock    = Sock
      },
      0   %% timeout jumps to accept
   };

init([Config, {connect, Node}]) ->
   {ok,
      'CONNECTING',
      #fsm{
         passive = false, 
         node    = Node,
         config  = Config, 
         recon   = undefined,
         sock    = undefined
      },
      0   %% timeout jumps to connect
   }.
   
%%
%%
'CONNECTING'(timeout, State) ->
   {Host, Port, Msg} = ws_con_req(State#fsm.node, State#fsm.config),
   % TODO: connect timeout
   case gen_tcp:connect(Host, Port, ?SOCK_OPTS, ?T_TCP_SOCK_CON) of
      {ok, Sock}  ->
         case State#fsm.recon of
            undefined -> ok;
            Ref       -> gen_fsm:cancel_timer(Ref)
         end,
         % connected send a message
         gen_tcp:send(Sock, Msg),
         {next_state, 'HANDSHAKE', State#fsm{sock = Sock, recon = undefined}, ?T_WEB_SOCK_CON};
      {error, R}  ->
         case State#fsm.recon of
            undefined ->
               Ref = gen_fsm:send_event_after(?T_PEER_RECON, peer_unavailable),
               {next_state, 'CONNECTING', State#fsm{recon = Ref}, 0};
            _         -> 
               {next_state, 'CONNECTING', State, 0}
         end
   end;
'CONNECTING'(peer_unavailable, State) ->
   error_logger:error_report([
      no_connection, 
      {state, 'CONNECTING'},
      {node, State#fsm.node}
   ]),
   ets:delete(ek_nodes, State#fsm.node),
   {stop, normal, State}.
   
%%
%%
'LISTEN'(timeout, State) ->
   {ok, Sock} = gen_tcp:accept(State#fsm.sock), 
   {ok, Pid}  = ek_ws_sup:accept(State#fsm.sock),
   ok = gen_tcp:controlling_process(State#fsm.sock, Pid),
   {next_state, 'HANDSHAKE', State#fsm{sock = Sock}, ?T_WEB_SOCK_CON}.
%%'LISTEN'(_Evt, State) ->
%%   {next_state, 'LISTEN', State}.
   
%%
%%
'HANDSHAKE'(timeout, State) ->
   error_logger:error_report([
      timeout, 
      {state, 'HANDSHAKE'},
      {node, State#fsm.node}
   ]),
   % handshake timeout
   case State#fsm.passive of
      true  -> {next_state, 'HANDOVER',   State, ?T_PEER_RECON};
      false -> {next_state, 'CONNECTING', State, 0}
   end.
%%'HANDSHAKE'(_Evt, State) ->
%%   {next_state, 'HANDSHAKE', State}.
   
%%
%%
'CONNECTED'({msg, Uri, Msg}, State) ->
   % TODO: handle msg transmission over stream protocol
   Pckt = term_to_binary({msg, Uri, Msg}),
   ok  = gen_tcp:send(State#fsm.sock, Pckt),
   {next_state, 'CONNECTED', State}.
   
%%
%%
'HANDOVER'(timeout, State) ->
   % connection handover to another process is failed (node disconnected)
   ets:delete(ek_nodes, State#fsm.node),
   error_logger:error_report([
      no_connection, 
      {state, 'HANDOVER'},
      {node, State#fsm.node}
   ]),
   {stop, normal, State};
'HANDOVER'(handover, State) ->
   {stop, normal, State}.
%%'HANDOVER'(_Evt, State) ->
%%   {next_state, 'HANDOVER', State}.
   
handle_event(_Evt, Name, State) ->
   {next_state, Name, State}.

handle_sync_event(_Req, _From, Name, State) ->
   {reply, {error, not_supported}, Name, State}.   

%% Sock data
handle_info({tcp, Sock, Data}, 'HANDSHAKE', State) ->
   Node = ws_con_ind(Data, State),
   % Hanshake is over
   case State#fsm.passive of
     true  ->
        case ets:lookup(ek_nodes, Node) of
           [{Node, Pid}] ->
              % node is controlled (perform handover)
              gen_fsm:send_event(Pid, handover);
           _             ->
              ok
        end;
     false -> 
        ok
   end,   
   ets:insert(ek_nodes, {Node, self()}),
   error_logger:info_report([join, {Node, self()}]),
   {next_state, 'CONNECTED', State#fsm{node = Node}};
handle_info({tcp, Sock, Data}, 'CONNECTED', State) ->
   %%error_logger:info_msg('Data: ~p~n', [Data]),
   Msg = binary_to_term(Data),
   dispatch_msg(Msg),
   %%error_logger:info_msg('Msg: ~p~n', [Msg]),
   {next_state, 'CONNECTED', State};
handle_info({tcp, Sock, Data}, Name, State) ->
   %%error_logger:info_msg('Data ~p: ~p~n', [Name, Data]),
   {next_state, Name, State};   
   
%%
%% Sock closed
handle_info({tcp_closed, Sock}, Name, State) ->
   error_logger:error_report([
      tcp_closed, 
      {state, Name},
      {node, State#fsm.node}
   ]),
   case State#fsm.passive of
      true  -> {next_state, 'HANDOVER',   State, ?T_PEER_RECON};
      false -> {next_state, 'CONNECTING', State, 0}
   end;
   
%%
%% Sock error
handle_info({tcp_error, Sock, Reason}, Name, State) ->
   error_logger:error_report([
      tcp_error, 
      {state, Name},
      {node, State#fsm.node},
      {error, Reason}
   ]),
   case State#fsm.passive of
      true  -> {next_state, 'HANDOVER',   State, ?T_PEER_RECON};
      false -> {next_state, 'CONNECTING', State, 0}
   end;   

   
handle_info(_Msg, Name, State) ->
   {next_state, Name, State}.
   
terminate(_Reason, Name, State) ->
   ok.
   
code_change(_OldVsn, Name, State, _Extra) ->
   {ok, Name, State}.   
   
   
%%%------------------------------------------------------------------
%%%
%%% Private
%%%
%%%------------------------------------------------------------------
   
%%
%% Connection request return Host, Port, Msg
ws_con_req(Node, Config) ->
   Uri  = ek_uri:new(Node),
   Host = proplists:get_value(host, Uri),
   Port = proplists:get_value(port, Uri),
   BPrt = list_to_binary(integer_to_list(Port)),
   [{self, Self}] = ets:lookup(ek_nodes, self),
   BSlf = list_to_binary(Self),
   case proplists:get_value(proxy, Config) of
      undefined      ->
         
         Req   = <<
            "GET / HTTP/1.1\r\n", 
            "Host: ", Host/binary, ":", BPrt/binary, "\r\n",
            "Peer: ", BSlf/binary, "\r\n",
            "Connection: Upgrade\r\n",
            "Upgrade: websocket\r\n\r\n"
         >>,
         {binary_to_list(Host), Port, Req};
      {PHst, PPrt} ->
         Req  = <<
            "CONNECT ", Host/binary, ":", BPrt/binary, " HTTP/1.1\r\n",
            "Host: ", Host/binary, ":", BPrt/binary, "\r\n",
            "Peer: ", BSlf/binary, "\r\n",
            "Connection: Upgrade\r\n",
            "Upgrade: websocket\r\n\r\n"
         >>,
         {PHst, PPrt, Req}
   end.
   
ws_con_rsp() ->  
   <<
      "HTTP/1.1 101 Switching Protocols\r\n",
      "Upgrade: websocket\r\n",
      "Connection: Upgrade\r\n"
   >>.

%%
%%
ws_con_ind(<<"GET / HTTP/1.1\r\n", Rest/binary>>, State) ->
   ok    = gen_tcp:send(State#fsm.sock, ws_con_rsp()),
   BPeer = get_peer(Rest, undefined, <<>>),
   binary_to_list(BPeer);
   
ws_con_ind(<<"HTTP/1.1 101 Switching Protocols\r\n", Rest/binary>>, State) ->   
   State#fsm.node.


%%
%%
get_peer(<<"Peer: ", Str/binary>>, State, Acc) ->
   get_peer(Str, peer, <<>>);
get_peer(<<"\r\n", Str/binary>>, peer, Acc) ->
   Acc;
get_peer(<<H:8, T/binary>>, State, Acc) ->
   get_peer(T, State, <<Acc/binary, H>>).
   
   
%%
dispatch_msg({msg, Uri, Data}) ->
   ok; 
   
dispatch_msg(_) -> 
   ok.

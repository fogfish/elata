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
-include("include/ek.hrl").

%%
%% WebSocket transport for Erlang Cluster
%% fsm:
%%   - LISTEN:     Listens incoming connection (passive)
%%   - CONNECTING: Tries to connect (active)
%%   - HANDSHAKE:  WS Connection setup
%%   - CONNECTED:
%%
%% timers:
%%   - tcp_sock
%%   - web_sock


%% TODO:
%%   - message separator


-export([
   start_link/2,
   %% gen_fsm
   init/1,
   'LISTEN'/2,
   'CONNECTING'/2,
   'HANDSHAKE'/2,
   'CONNECTED'/2,
   handle_event/3,
   handle_sync_event/4,
   handle_info/3,
   terminate/3,
   code_change/4
]).

-record(fsm, {
   node,        % remote peer id
   config,      % cluster configuration
   sock,        % remote peer socket
   q            % message queue 
}).

-define(SOCK_OPTS, [{active, true}, {mode, binary}]).

-define(T_TCP_SOCK_CON,  20000).  %% timer for tcp socket connection
-define(T_WEB_SOCK_CON,  20000).  %% timer for web socket connection
-define(T_PEER_INACTIVE, 60000).  %% timer to node reconnect

%%
%% debug macro
-ifdef(DEBUG).
-define(DEBUG(M), error_logger:info_report(M)).
-else.
-define(DEBUG(M), true).
-endif.


%%
%%
start_link(Config, {listen, Uri}) ->
   gen_fsm:start_link(?MODULE, [Config, {listen, Uri}], []);
start_link(Config, {accept, Sock}) ->
   gen_fsm:start_link(?MODULE, [Config, {accept, Sock}], []);
start_link(Config, {connect, Node}) ->
   gen_fsm:start_link(?MODULE, [Config, {connect, Node}], []).
   
init([Config, {listen, Uri}]) ->
   % start listen of incomming request(s) 
   U    = ek_uri:new(Uri),
   Port = proplists:get_value(port, U),   
   {ok, Sock} = gen_tcp:listen(Port, ?SOCK_OPTS),
   % register itself into node list
   ets:insert(ek_nodes, #ek_node{uri=Uri, pid=self}),
   {ok, 
      'LISTEN', 
       #fsm{
          node    = undefined,
          config  = Config, 
          sock    = Sock,
          q       = []
       }, 
       0   %% timeout jumps to accept
    };
   
init([Config, {accept, Sock}]) ->
   % incomming connection has been established, 
   % transfer ownership of listen socket to new process
   % and accept connections
   {ok, 
      'LISTEN', 
      #fsm{
         node    = undefined,
         config  = Config, 
         sock    = Sock,
         q       = []
      },
      0   %% timeout jumps to accept
   };

init([Config, {connect, Node}]) ->
   % connect request is instantiated
   {ok,
      'CONNECTING',
      #fsm{
         node    = Node,
         config  = Config, 
         sock    = undefined,
         q       = []
      },
      0   %% timeout jumps to connect
   }.
   
%%-------------------------------------------------------------------   
%%
%% Connect to remote peer (Socket phase)
%%
%%-------------------------------------------------------------------
'CONNECTING'(timeout, State) ->
   case ws_check_peer(State#fsm.node) of
      true  ->
         {stop, {error, already_exists}, State};
      false ->
         % initiate connection with remote peer
         {Host, Port, Msg} = ws_con_req(State#fsm.node, State#fsm.config),
         ?DEBUG([connecting, {node, State#fsm.node}]),
         case gen_tcp:connect(Host, Port, ?SOCK_OPTS, ?T_TCP_SOCK_CON) of
            {ok, Sock}  ->
               % socket connected, send a message
               gen_tcp:send(Sock, Msg),
               {next_state, 
                  'HANDSHAKE', 
                  State#fsm{sock = Sock}, 
                  ?T_WEB_SOCK_CON
               };
            {error, R}  ->
               % unable to connect try again
               {stop, {error, {socket, R}}, State}
         end   
   end;
'CONNECTING'({tx, Data}, State) ->   
   % node tries to send message while no connection, queue the message
   {next_state, 
      'CONNECTING', 
      State#fsm{q = State#fsm.q ++ [Data]}
   }.
   
%%-------------------------------------------------------------------   
%%
%% Listen remote peer (Socket phase)
%%
%%-------------------------------------------------------------------
'LISTEN'(timeout, State) ->
   {ok, Sock} = gen_tcp:accept(State#fsm.sock), 
   {ok, Pid}  = ek_ws_sup:accept(State#fsm.sock),
   ok = gen_tcp:controlling_process(State#fsm.sock, Pid),
   {next_state, 
      'HANDSHAKE', 
      State#fsm{sock = Sock}, 
      ?T_WEB_SOCK_CON
   }.

%%-------------------------------------------------------------------   
%%
%% Handhshake with remote peer (Web Socket phase)
%%
%%-------------------------------------------------------------------
'HANDSHAKE'(timeout, State) ->
   {stop, {error, timeout}, State};
'HANDSHAKE'({tx, Data}, State) ->   
   % node tries to send message while no connection, queue the message
   {next_state, 
      'CONNECTING', 
      State#fsm{q = State#fsm.q ++ [Data]}
   }.


%%-------------------------------------------------------------------   
%%
%% Connected 
%%
%%-------------------------------------------------------------------
'CONNECTED'(timeout, State) ->
   % flush queue
   lists:foreach(
      fun(Data) -> gen_tcp:send(State#fsm.sock, Data) end,
      State#fsm.q
   ),
   {next_state, 'CONNECTED', State#fsm{q = []}};
'CONNECTED'({tx, Data}, State) ->
   % TODO: handle msg transmission over stream protocol
   gen_tcp:send(State#fsm.sock, Data),
   {next_state, 'CONNECTED', State}.

handle_event(_Evt, Name, State) ->
   {next_state, Name, State}.

handle_sync_event(_Req, _From, Name, State) ->
   {reply, {error, not_supported}, Name, State}.   

%%-------------------------------------------------------------------   
%%
%% Handle incoming socket data 
%%
%%-------------------------------------------------------------------
handle_info({tcp, _Sock, Data}, 'HANDSHAKE', State) ->
   % parse incoming indication
   case ws_con_ind(Data) of
      {ok,  Node} ->
         % accept connection request from remote peer
         gen_tcp:send(State#fsm.sock, ws_con_rsp()),
         ws_connect_peer(Node),
         ?DEBUG([join, {Node, self()}]),
         {next_state, 'CONNECTED', State#fsm{node = Node}};
      ok          ->
         % peer is connected
         ws_connect_peer(State#fsm.node),
         ?DEBUG([join, {State#fsm.node, self()}]),
         {next_state, 'CONNECTED', State};
      {error, {already_exists, Node}} ->
         % decline connection request from peer
         gen_tcp:send(State#fsm.sock, ws_con_err()),
         error_logger:error_report([already_exists, {Node, self()}]),
         {stop, {error, already_exists}, State};
      {error, Err}      ->
         % peer error
         error_logger:error_report([declined, {State#fsm.node, self()}]),
         {stop, {error, Err}, State}
   end;
handle_info({tcp, _Sock, Data}, 'CONNECTED', State) ->
   ek_prot:recv(Data),
   {next_state, 'CONNECTED', State};
handle_info({tcp, _Sock, _Data}, Name, State) ->
   {next_state, Name, State};   

%%-------------------------------------------------------------------   
%%
%% Handle socket signalling
%%
%%-------------------------------------------------------------------
   
%%
%% Sock closed
handle_info({tcp_closed, _Sock}, _Name, State) ->
   {stop, {error, tcp_closed}, State};
   
%%
%% Sock error
handle_info({tcp_error, _Sock, Reason}, _Name, State) ->
   {stop, {error, {tcp_error, Reason}}, State};   
   
handle_info(_Msg, Name, State) ->
   {next_state, Name, State}.

%% 
%% terminate
terminate(normal, Name, State) ->
   ?DEBUG([
      {error, normal},
      {state, Name},
      {node, State#fsm.node}
   ]),
   ets:delete(ek_nodes, State#fsm.node),
   ek_evt:leave(State#fsm.node),
   ok;
terminate({error, already_exists}, Name, State) -> 
   error_logger:error_report([
      {error, already_exists}, 
      {state, Name},
      {node, State#fsm.node}
   ]),
   ok;
terminate({error, Err}, Name, State) ->
   error_logger:error_report([
      {error, Err}, 
      {state, Name},
      {node, State#fsm.node}
   ]),
   ets:delete(ek_nodes, State#fsm.node),
   ek_evt:leave(State#fsm.node),
   ok.
   
code_change(_OldVsn, Name, State, _Extra) ->
   {ok, Name, State}.   
   
%%%------------------------------------------------------------------
%%%
%%% Private
%%%
%%%------------------------------------------------------------------
   

%%%
%%% Internal protocol primitives
%%%

%%
%% Check if peer is already connected
ws_check_peer(Node) ->
   case ets:match_object(ek_nodes, {ek_node, Node, '_'}) of
      []  -> false;
      [N] -> 
         if
            N#ek_node.pid =/= self() -> true;
            N#ek_node.pid =:= self() -> false
         end
   end.
   
ws_connect_peer(Node) ->
   % Hanshake is over
   ek_evt:join(Node),
   ets:insert(ek_nodes, #ek_node{uri=Node, pid=self()}).
        

%%
%% Connection request return Host, Port, Msg
ws_con_req(Node, Config) ->
   % parse Node uri for host & port
   Uri  = ek_uri:new(Node),
   Host = proplists:get_value(host, Uri),
   Port = proplists:get_value(port, Uri),
   BPrt = list_to_binary(integer_to_list(Port)),
   Self = list_to_binary(ek:node()),
   case proplists:get_value(proxy, Config) of
      undefined      ->
         % proxy is not defined, direct connection with remote peer requested
         Req   = <<
            "GET / HTTP/1.1\r\n", 
            "Host: ", Host/binary, ":", BPrt/binary, "\r\n",
            "Peer: ", Self/binary, "\r\n",
            "Connection: Upgrade\r\n",
            "Upgrade: websocket\r\n",
            "\r\n"
         >>,
         {binary_to_list(Host), Port, Req};
      {PHst, PPrt} ->
         % proxy is defined, establish connection throught it
         Req  = <<
            "CONNECT ", Host/binary, ":", BPrt/binary, " HTTP/1.1\r\n",
            "Host: ", Host/binary, ":", BPrt/binary, "\r\n",
            "Peer: ", Self/binary, "\r\n",
            "Connection: Upgrade\r\n",
            "Upgrade: websocket\r\n\r\n"
         >>,
         {PHst, PPrt, Req}
   end.

%%
%%
ws_con_rsp() ->  
   <<
      "HTTP/1.1 101 Switching Protocols\r\n",
      "Upgrade: websocket\r\n",
      "Connection: Upgrade\r\n",
      "\r\n"
   >>.

ws_con_err() ->
   <<
      "HTTP/1.1 500 Internal Server Error\r\n",
      "Connection: close\r\n",
      "\r\n"
   >>.
   
%%
%% Handles incoming messages 
ws_con_ind(<<"GET / HTTP/1.1\r\n", Rest/binary>>) ->
   Node  = get_peer(Rest, undefined, <<>>),
   case ws_check_peer(Node) of
      true  -> {error, {already_exists, Node}};
      false -> {ok, Node}
   end;
   
ws_con_ind(<<"HTTP/1.1 101 Switching Protocols\r\n", _/binary>>) ->   
   ok;
ws_con_ind(<<"HTTP/1.1 500 Internal Server Error\r\n", _/binary>>) ->
   {error, 500}.

%%
%%
get_peer(<<"Peer: ", Str/binary>>, _State, _Acc) ->
   get_peer(Str, peer, <<>>);
get_peer(<<"\r\n", _/binary>>, peer, Acc) ->
   binary_to_list(Acc);
get_peer(<<H:8, T/binary>>, State, Acc) ->
   get_peer(T, State, <<Acc/binary, H>>).
   


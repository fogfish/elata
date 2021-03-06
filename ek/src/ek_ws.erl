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
%% ek socket protocol
%%


-export([
   start_link/2,
   % gen_fsm
   init/1,
   'IDLE'/2, 
   'LISTEN'/2,
   'HANDSHAKE'/2,
   'CONNECTED'/2,
   handle_event/3,
   handle_sync_event/4,
   handle_info/3,
   terminate/3,
   code_change/4
]).


-record(fsm, {
   peer,
   proxy,
   sock,
   sideA = undefined
}).

-define(SOCK_OPTS, [{active, true}, {mode, binary}]).
-define(T_TCP_LISTEN,    30000).  %% timer to repeat failed socket listen
-define(T_TCP_SOCK_CON,  20000).  %% timer for tcp socket connection
-define(T_WEB_SOCK_CON,  20000).  %% timer for web socket connection

%%
%% debug macro
-ifdef(DEBUG).
-define(DEBUG(M), error_logger:info_report([{?MODULE, self()}] ++ M)).
-else.
-define(DEBUG(M), true).
-endif.

%%
%%
start_link(Config, {listen, Uri})  ->
   gen_fsm:start_link({local, ek_sock_listener}, ?MODULE, [Config, {listen, Uri}], []);
start_link(Config, {handshake, Sock}) ->
   gen_fsm:start_link(?MODULE, [Config, {handshake, Sock}], []);
start_link(Config, {connect, Uri, SideA}) ->
   gen_fsm:start_link(?MODULE, [Config, {connect, Uri, SideA}], []).

   
init([Config, {listen, Uri}]) ->
   {ok, 
      'IDLE', 
       #fsm{
          peer    = Uri,
          proxy   = proplists:get_value(proxy, Config), 
          sock    = undefined
       }, 
       0   %% timeout jumps to idle loop (trying to listen)
    };   
    
init([Config, {handshake, Sock}]) ->
   case inet:sockname(Sock) of
      {ok, _} ->
         ?DEBUG([{socket, handshake}]),
         {ok,
      'HANDSHAKE',
      #fsm{
         peer    = undefined,
         proxy   = proplists:get_value(proxy, Config), 
         sock    = Sock 
      },
      ?T_WEB_SOCK_CON
         };
      {error, _} ->
         % process is restarted by supervisor, no actions is required
         {stop, normal}
   end;
   
init([Config, {connect, Uri, SideA}]) ->
   ?DEBUG([{socket, connect}, {peer, Uri}]),
   {ok, Sock} = case proplists:get_value(proxy, Config) of
      undefined    ->
         gen_tcp:connect(
            ek_uri:get(host, Uri),
            ek_uri:get(port, Uri),
            ?SOCK_OPTS,
            ?T_TCP_SOCK_CON
         );
      Proxy ->
         gen_tcp:connect(
            ek_uri:get(host, Proxy),
            ek_uri:get(port, Proxy),
            ?SOCK_OPTS,
            ?T_TCP_SOCK_CON
         )
   end,   
   {ok,
      'CONNECTED',
      #fsm{
         peer  = Uri,
         proxy = proplists:get_value(proxy, Config), 
         sock  = Sock,
         sideA = SideA
      },
      0   %% timeout jump to connect loop
   }.

%%
%% creates listen socket
%%
'IDLE'(timeout, S) ->   
   ?DEBUG([{socket, listen}, {node, S#fsm.peer}]),
   case gen_tcp:listen(ek_uri:get(port, S#fsm.peer), ?SOCK_OPTS) of
      {ok, Sock} ->
         % TODO: fix listener identity based on ek scheme
         ek:register(ek_uri:set(schema, ek, S#fsm.peer), self()),
         {next_state, 'LISTEN', S#fsm{sock = Sock}, 0};
      {error, _} ->
         {next_state, 'IDLE', S, ?T_TCP_LISTEN}
   end.

%%
%% listening on port
%%
'LISTEN'(timeout, State) ->
   % accept loop
   {ok, Sock} = gen_tcp:accept(State#fsm.sock),
   ?DEBUG([{socket, accepted}, {port, State#fsm.peer}]),
   % trasfer control to a new process
   {ok, Pid} = ek_ws_sup:create({handshake, Sock}),
   ok = gen_tcp:controlling_process(Sock, Pid),
   {next_state, 'LISTEN', State, 0}.
   
   
   
'HANDSHAKE'(timeout, State) ->   
   % handshake is not accomplished
   {stop, {error, timeout}, State}.
   
'CONNECTED'(timeout, State) ->
   ?DEBUG([{socket, handshake}, {peer, State#fsm.peer}]),
   ok = gen_tcp:send(
      State#fsm.sock, 
      ws_con_req(State#fsm.peer, State#fsm.proxy)
   ),
   {next_state, 'HANDSHAKE', State, ?T_WEB_SOCK_CON}.
    
handle_event(_Evt, Name, State) ->
   {next_state, Name, State}.
 
handle_sync_event(_Req, _From, Name, State) ->
   {reply, {error, not_supported}, Name, State}.

%%
%% Data
handle_info({tcp, _Sock, Data}, 'HANDSHAKE', #fsm{peer = undefined} = State) ->
   % accepted socket / passive peer
   case Data of
      <<"GET / HTTP/1.1\r\n", Req/binary>> ->
         ?DEBUG([{socket, connected}]),
         ok = gen_tcp:send(
            State#fsm.sock,
            ws_con_rsp()
         ),
         Peer = x_node(Req),
         Pid = case ek:whereis(Peer) of
            undefined ->
               {ok, Proc} = ek_prot_sup:create(Peer),
               Proc;
            Proc       ->
               Proc
         end,
         ok = gen_tcp:controlling_process(State#fsm.sock, Pid),
         gen_fsm:send_event(Pid, {socket, State#fsm.sock}),
         timer:sleep(1000),
         {stop, normal, State};
      _ ->
         {next_state, 'HANDSHAKE', State}
   end;
   
handle_info({tcp, _Sock, Data}, 'HANDSHAKE', State) ->   
   % connected socket / active peer
   case Data of
      %
      <<"HTTP/1.1 101", _/binary>> ->
         ?DEBUG([{socket, connected}, {peer, State#fsm.peer}]),
         % transfer socket control to ek_prot (sideA)
         ok = gen_tcp:controlling_process(State#fsm.sock, State#fsm.sideA),
         gen_fsm:send_event(State#fsm.sideA, {socket, State#fsm.sock}),
         {stop, normal, State};
      <<"HTTP/1.1 200", _/binary>> ->   
         % stupid proxy cannot upgrade but TCP/IP peer is on 
         ?DEBUG([{socket, connected}, {peer, State#fsm.peer}]),
         gen_tcp:send(State#fsm.sock, ws_con_req(State#fsm.peer, undefined)),
         % transfer socket control to ek_prot (sideA)
         ok = gen_tcp:controlling_process(State#fsm.sock, State#fsm.sideA),
         gen_fsm:send_event(State#fsm.sideA, {socket, State#fsm.sock}),
         {stop, normal, State};
      _ ->
         ?DEBUG([{socket, error}, {data, Data}]),
         {next_state, 'HANDSHAKE', State} 
   end;
 
%%
%% Sock closed
handle_info({tcp_closed, _Sock}, _Name, State) ->
   ?DEBUG([{socket, closed}, {peer, State#fsm.peer}]),
   {stop, {error, tcp_closed}, State};
   
%%
%% Sock error
handle_info({tcp_error, _Sock, Reason}, _Name, State) ->
   ?DEBUG([{socket, {tcp_error, Reason}}, {peer, State#fsm.peer}]),
   {stop, {error, {tcp_error, Reason}}, State}; 
   
handle_info(Msg, Name, State) ->
   ?DEBUG([{msg, Msg}, {state, Name}]),
   {next_state, Name, State}.
   
   
%%
%%
terminate(Reason, Name, State) ->
   ?DEBUG([{socket, terminated}, {reason, Reason}, {peer, State#fsm.peer}]),
   ok.
   
code_change(_OldVsn, Name, State, _Extra) ->
   {ok, Name, State}.   
   
   
   
%%%------------------------------------------------------------------
%%%
%%% Private
%%%
%%%------------------------------------------------------------------

%%
%% Connect request primitive
ws_con_req(Peer, undefined) ->
   % w/o proxy
   Host = list_to_binary(ek_uri:get(host, Peer)),
   Port = list_to_binary(
      integer_to_list(ek_uri:get(port, Peer))
   ),
   Node = ek_uri:to_binary(ek_uri:set(authority, ek:node(), Peer)),
   <<
      "GET / HTTP/1.1\r\n", 
      "Host: ", Host/binary, ":", Port/binary, "\r\n",
      "X-Node: ", Node/binary , "\r\n",
      "Connection: Upgrade\r\n",
      "Upgrade: websocket\r\n",
      "\r\n"
   >>;
   
ws_con_req(Peer, Proxy) ->
   % with proxy
   Host = list_to_binary(ek_uri:get(host, Peer)),
   Port = list_to_binary(
      integer_to_list(ek_uri:get(port, Peer))
   ),
   Node = ek_uri:to_binary(ek_uri:set(authority, ek:node(), Peer)),
   Req = <<
      "CONNECT ", Host/binary, ":", Port/binary, " HTTP/1.1\r\n",
      "Host: ", Host/binary, ":", Port/binary, "\r\n",
      "Proxy-Connection: keep-alive\r\n",
      "X-Node: ", Node/binary , "\r\n",
      "Connection: Upgrade\r\n",
      "Upgrade: websocket\r\n", 
      "\r\n"
   >>. 
   
ws_con_rsp() ->
   <<
      "HTTP/1.1 101 Switching Protocols\r\n",
      "Upgrade: websocket\r\n",
      "Connection: Upgrade\r\n",
      "\r\n"
   >>.
   
%%
%%
x_node(Msg) ->
   x_node(Msg, undefined, <<>>).
x_node(<<"X-Node: ", Str/binary>>, _State, _Acc) ->
   x_node(Str, xnode, <<>>);
x_node(<<"\r\n", _/binary>>, xnode, Acc) ->
   binary_to_list(Acc);
x_node(<<H:8, T/binary>>, State, Acc) ->
   x_node(T, State, <<Acc/binary, H>>);
x_node(<<>>, _, _) ->
   undefined.
   
   
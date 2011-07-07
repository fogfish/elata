%%
%%   Copyright (c) 2011, Nokia Corporation
%%   All rights reserved.
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
-module(agt_probe).
-author(dmitry.kolesnikov@nokia.com).
-include("include/def.hrl").

%%
%% Functionals to measure a latency each functional is sequence of 
%% atomic operations. They return value must be a list of tuples 
%% that represent latency for atomic operation(s)
%%

-export([
   tcp/2, 
   ssl/2, 
   http/2
]).

-define(TCP_OPTS, [binary, {packet, 0}, {active, false}]).
-define(SSL_OPTS, []).
-define(TCP_PORT, 80).

%%
%% Latency to establish TCP/IP connection
tcp(#uri{host = Host, port = Port}, _Opts)  ->
   {Ttcp, {ok, Tcp}} = timer:tc(
      gen_tcp, 
      connect, 
      [binary_to_list(Host), Port, ?TCP_OPTS]
   ),
   gen_tcp:close(Tcp),
   {ok, [{tcp, Ttcp}, {uri, Ttcp}]}.

%%
%% Latency to establish SSL connection
ssl(#uri{host = Host, port = Port}, _Opts) ->
   {Ttcp, {ok, Tcp}} = timer:tc(
      gen_tcp, 
      connect, 
      [binary_to_list(Host), Port, ?TCP_OPTS]
   ),
   {Tssl, {ok, Ssl}} = timer:tc(
      ssl,
      connect, 
      [Tcp, ?SSL_OPTS]
   ),
   ssl:close(Ssl),
   {ok, [{tcp, Ttcp}, {ssl, Tssl}, {uri, Ttcp + Tssl}]}.

%%
%% Latency of HTTP(S) and underlying protocols
http(Uri, Opts)  ->
   {Host, Port} = get_host(Uri#uri.host, Uri#uri.port, Opts),   
   % connect to host
   {Ttcp, {ok, Tcp}} = timer:tc(
      gen_tcp, 
      connect, 
      [binary_to_list(Host), Port, ?TCP_OPTS]
   ),
   {Tssl, Mod, Sock} = case Uri#uri.schema of
      https ->
         {Time, {ok, Ssl}} = timer:tc(ssl, connect, [Tcp, ?SSL_OPTS]),
         {Time, ssl, Ssl};
      http  ->
         {0, gen_tcp, Tcp}
   end,
   % send out http get   
   ok = Mod:send(Sock, http_get(Uri, Opts)),
   {Ttfb, {ok, Pckt}} = timer:tc(Mod, recv,  [Sock, 0]),
   {Ttmr, {ok, _Rsp}} = timer:tc(fun http_recv/3, [Mod, Sock, [Pckt]]),
   Mod:close(Sock),
   {ok,
      [
      {tcp,  Ttcp},
      {ssl,  Tssl},
      {ttfb, Ttfb},
      {ttmr, Ttmr},
      {uri,  Ttcp + Tssl + Ttfb + Ttmr}
      ]
   }.
  
%%%------------------------------------------------------------------   
%%%
%%% Private functions
%%%
%%%------------------------------------------------------------------   

%% Return host and port to connect
get_host(Host, Port, Opts) ->
   case proplists:get_value(proxy, Opts, undefined) of
      undefined -> {Host, Port};
      Proxy     -> Proxy
   end.   
   
%%
%% return HTTP GET
%% Uri 
%% Heads - list of user defined headers
http_get(#uri{path = Path} = U, Opts) ->
   case proplists:get_value(proxy, Opts, undefined) of
      undefined ->
         Request = <<"GET ", Path/binary, " HTTP/1.1\r\n">>;
      _         ->
         FPath = uri_util:to_binary(U),
         Request = <<"GET ", FPath/binary, " HTTP/1.1\r\n">>
   end,         
   Heads = proplists:get_value(headers, Opts, []),
   Common= http_heads_common(U, Opts),
   Custom=list_to_binary([<<X/binary, "\r\n">> || X <- Heads]),
   <<Request/binary, Common/binary, Custom/binary, "\r\n">>.
  
%%
%% return a list of common HTTP headers
http_heads_common(U, Opts) ->
   {Host, Port} = get_host(U#uri.host, U#uri.port, Opts),
   Bport = list_to_binary(integer_to_list(Port)),
   <<
      "Connection: close\r\n",
      "Accept: */*\r\n",
      "Host: ", Host/binary, ":", Bport/binary, "\r\n" 
   >>. 

%%
%% receives http data
%% Mod  - atom() module responsibe for socket management (gen_tcp or ssl)
%% Sock - Socket instance
%% Data - list() recieved data so far
http_recv(Mod, Sock, Data) ->
   {_Tpckt, Packet} = timer:tc(Mod, recv, [Sock, 0]),
   case Packet of
      {ok, Chunk} ->
         http_recv(Mod, Sock, [Data, Chunk]);
      {error, closed} ->
         {ok, list_to_binary(Data)}
  end.
  

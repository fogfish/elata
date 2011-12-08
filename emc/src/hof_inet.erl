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
-module(hof_inet).
-author(sergey.boldyrev@nokia.com).
-author(dmitry.kolesnikov@nokia.com).

%%
%% High order-function: Internet
%%

-export([
   dns/1,
   %TODO: dns_full
   tcp/2,
   tcp/3,
   ssl/3,
   statistic/1,
   close/1
]).

%%
%% resolve IP address of URI authority
dns({Schema,_,Path} = Uri) ->
   {ok, IP} = inet:getaddr(binary_to_list(ek_uri:host(Uri)), inet),
   {ok, [IP, Uri]}.

%%
%% Establish TCP/IP connection, and measure its latency
tcp({_,_,_} = Uri, Opts) ->
   {ok, IP} = inet:getaddr(binary_to_list(ek_uri:host(Uri)), inet),
   tcp(IP, Uri, Opts).
tcp(IP, {_,_,_} = Uri, undefined) ->   
   tcp(IP, Uri, []);
tcp(IP, {_,_,_} = Uri, Opts) ->
   {Host, Port} = case proplists:get_value(proxy, Opts) of
      undefined ->
         {IP, ek_uri:port(Uri)};
      Proxy     ->
         {binary_to_list(ek_uri:host(Proxy)), ek_uri:port(Proxy)}
   end,
   {ok, Tcp} = gen_tcp:connect(
      Host, Port,
      [binary, {packet, 0}, {active, false}]
   ),
   {ok, [{gen_tcp, Tcp}, Uri, Opts]}.
   
%%
%% Establish SSL connection and measures its latency
ssl({gen_tcp, Sock}, {ssl,_,_} = Uri, Opts) ->
   {ok, Ssl} = ssl:connect(Sock, []),
   {ok, [{ssl, Ssl}, Uri, Opts]};
ssl({gen_tcp, Sock}, {https,_,_} = Uri, Opts) ->   
   {ok, Ssl} = ssl:connect(Sock, []),
   {ok, [{ssl, Ssl}, Uri, Opts]};
ssl(Sock, Uri, Opts) ->
   {ok, [Sock, Uri, Opts]}.
  
%%
%% Reports statistic on socket usage
statistic({gen_tcp, Sock}) ->
   {ok, Stat} = inet:getstat(Sock, [recv_avg, recv_cnt, recv_oct]),
   {ok, [{gen_tcp, Sock}, Stat]};
statistic({ssl, Sock}) ->
   % TODO: statistic for ssl sockets
   {ok, [{ssl, Sock}, []]}.

%%
%%
close({Mod, Sock}) ->
   Mod:close(Sock),
   {ok, []}.


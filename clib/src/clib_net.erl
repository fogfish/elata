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
-module(clib_net).
-author(sergey.boldyrev@nokia.com).
-author(dmitry.kolesnikov@nokia.com).

%%
%% Computation primitives: network 
%%
%% Each primitive follows the interface notation
%%    fun(Input, State) -> {ok, Output, State}
%%    Input  = list() of input arguments [Arg1, Arg2, ...]
%%    Output = list() of output results 
%%    State  = tuple() transfered state 
%% 

-export([
   uri_parse/2,
   tcp_connect/2,
   tcp_close/2,
   ssl_connect/2,
   ssl_close/2
]).

%%
%% Uri Parser
uri_parse([Uri], _) ->
   {ok, [ek_uri:new(Uri)]}.

%%
%% Establish TCP/IP connection, and measure its latency
tcp_connect([Uri],     _)  ->
   tcp_connect([Uri, []], undefined);
tcp_connect([Uri, Ds], _) ->
   {Ttcp, {ok, Tcp}} = timer:tc(
      gen_tcp, 
      connect, 
      [
         binary_to_list(proplists:get_value(host, Uri)), 
         proplists:get_value(port, Uri), 
         [binary, {packet, 0}, {active, false}]
      ]
   ),
   {ok, [Uri, [{tcp, Ttcp} | Ds]], {net, Tcp}}.

%%
%% 
tcp_close([Uri, Ds], {net,Sock}) ->
   gen_tcp:close(Sock),
   {ok, [Uri, Ds], {}}.
   
%%
%% Establish SSL connection and measures its latency
ssl_connect([Uri, Ds], {net, Sock}) ->
   {Tssl, {ok, Ssl}} = timer:tc(
      ssl,
      connect, 
      [Sock, []]
   ),   
   {ok, [Uri, [{ssl, Tssl} | Ds]], {net, Ssl}}.
 
%%
%%
ssl_close([Uri, Ds], {net, Sock}) ->
   ssl:close(Sock),
   {ok, [Uri, Ds], {}}.
   
   
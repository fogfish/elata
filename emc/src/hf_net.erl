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
-module(hf_net).
-author(sergey.boldyrev@nokia.com).
-author(dmitry.kolesnikov@nokia.com).

%%
%% High order-function: network computation
%%

-export([
   tcp/2,
   ssl/3,
   close/1
]).

%%
%% Establish TCP/IP connection, and measure its latency
tcp({_,_,_} = Uri, Opts) ->
   {Host, Port} = case proplists:get_value(proxy, Opts) of
      undefined ->
         {ek_uri:host(Uri), ek_uri:port(Uri)};
      Proxy     ->
         {ek_uri:host(Proxy), ek_uri:port(Proxy)}
   end,
   {ok, Tcp} = gen_tcp:connect(
      binary_to_list(Host), 
      Port,
      [binary, {packet, 0}, {active, false}]
   ),
   [{gen_tcp, Tcp}, Uri, Opts].

   
%%
%% Establish SSL connection and measures its latency
ssl({gen_tcp, Sock}, {ssl,_,_} = Uri, Opts) ->
   {ok, Ssl} = ssl:connect(Sock, []),
   [{ssl, Ssl}, Uri, Opts];
ssl({gen_tcp, Sock}, {https,_,_} = Uri, Opts) ->   
   {ok, Ssl} = ssl:connect(Sock, []),
   [{ssl, Ssl}, Uri, Opts];
ssl(Sock, Uri, Opts) ->
   [Sock, Uri, Opts].

%%
%%
close({Mod, Sock}) ->
   Mod:close(Sock),
   [].


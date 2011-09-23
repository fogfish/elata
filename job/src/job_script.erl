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
-module(job_script).
-author(dmitry.kolesnikov@nokia.com).
-include("include/def.hrl").

%%
%% Executor of job scripts
%%

-export([
  execute/1
]).

% stub
execute(undefined) ->
   {ok, [], undefined};

execute(Job) when is_binary(Job) ->
   U = job_uri:new( Job ),
   case U#uri.schema of
      tcp  -> execute(U);
      ssl  -> execute(U);
      http -> execute(U);
      https-> execute(U);
      _    -> {error, not_supported}
   end;

%%
%% tcp/ip job
execute(#uri{schema = tcp} = Job) ->
   {Ttcp, {ok, Tcp}} = timer:tc(
      gen_tcp, 
      connect, 
      [
         binary_to_list(Job#uri.host), 
         Job#uri.port, 
         [binary, {packet, 0}, {active, false}]
      ]
   ),
   gen_tcp:close(Tcp),
   {ok, [{tcp, Ttcp}, {job, Ttcp}], undefined};

%%   
%% ssl job
execute(#uri{schema = ssl} = Job) ->
   {Ttcp, {ok, Tcp}} = timer:tc(
      gen_tcp, 
      connect, 
      [
         binary_to_list(Job#uri.host),
         Job#uri.port, 
         [binary, {packet, 0}, {active, false}]
      ]
   ),
   {Tssl, {ok, Ssl}} = timer:tc(
      ssl,
      connect, 
      [Tcp, []]
   ),
   ssl:close(Ssl),
   {ok, [{tcp, Ttcp}, {ssl, Tssl}, {job, Ttcp + Tssl}], undefined};

%%
%% http job
execute(#uri{schema = http} = Job) ->
   http_job(Job, []);

%%
%% https job
execute(#uri{schema = https} = Job) ->
   http_job(Job, []);

% job is a script   
execute(Job) ->
   U = job_uri:new( proplists:get_value(uri, Job) ),
   case U#uri.schema of
      tcp  -> execute(U);
      ssl  -> execute(U);
      http -> http_job(U, Job);
      https-> http_job(U, Job);
      _    -> {error, not_supported}
   end.   
   

%%%------------------------------------------------------------------   
%%%
%%% Private functions
%%%
%%%------------------------------------------------------------------

http_job(#uri{schema = http} = Uri, Job)  ->
   {Host, Port} = http_host(Uri, Job),
   % connect to host
   {Ttcp, {ok, Tcp}} = timer:tc(
      gen_tcp, 
      connect, 
      [
         binary_to_list(Host), 
         Port, 
         [binary, {packet, 0}, {active, false}]
      ]
   ),
   gen_tcp:send(Tcp, http_get_req(Uri, Job)),
   {Ttfb, {ok, Pckt}} = timer:tc(gen_tcp, recv,   [Tcp, 0]),
   {Ttmr, {ok, Data}} = timer:tc(fun http_recv/3, [gen_tcp, Tcp, [Pckt]]),
   gen_tcp:close(Tcp),
   {ok,
      [
      {tcp,  Ttcp},
      {ttfb, Ttfb},
      {ttmr, Ttmr},
      {job,  Ttcp + Ttfb + Ttmr}
      ],
      Data
   };

http_job(#uri{schema = https} = Uri, Job) ->
   {Host, Port} = http_host(Uri, Job),
   % connect to host
   {Ttcp, {ok, Tcp}} = timer:tc(
      gen_tcp, 
      connect, 
      [
         binary_to_list(Host), 
         Port, 
         [binary, {packet, 0}, {active, false}]
      ]
   ),
   {Tssl, {ok, Ssl}} = timer:tc(
      ssl,
      connect, 
      [Tcp, []]
   ),
   ssl:send(Ssl, http_get_req(Uri, Job)),
   {Ttfb, {ok, Pckt}} = timer:tc(ssl, recv,   [Ssl, 0]),
   {Ttmr, {ok, Data}} = timer:tc(fun http_recv/3, [ssl, Ssl, [Pckt]]),
   ssl:close(Ssl),
   {ok,
      [
      {tcp,  Ttcp},
      {ssl,  Tssl},
      {ttfb, Ttfb},
      {ttmr, Ttmr},
      {job,  Ttcp + Tssl + Ttfb + Ttmr}
      ],
      Data
   }.   

http_host(#uri{host = Host, port = Port}, Opts) ->
   case proplists:get_value(proxy, Opts, undefined) of
      undefined -> {Host, Port};
      Proxy     -> Proxy
   end.   
   
http_get_req(Uri, Job) ->
   % host port
   {Host, Prt} = http_host(Uri, Job),
   Port = list_to_binary(integer_to_list(Prt)),
   % path
   Path = case proplists:is_defined(proxy, Job) of
      false -> Uri#uri.path;
      true  -> job_uri:to_binary(Uri)
   end,
   % job headers
   Head = case proplists:get_value(header, Job) of
      undefined -> <<>>;
      List      -> list_to_binary([<<H/binary, ": ", V/binary, "\r\n">> || {H, V} <- List])
  end,    
   <<
      "GET ", Path/binary, " HTTP/1.1\r\n",
      "Connection: close\r\n",
      "Accept: */*\r\n",
      "Host: ", Host/binary, ":", Port/binary, "\r\n",
      Head/binary,
      "\r\n"
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
   
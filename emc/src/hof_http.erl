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
-module(hof_http).
-author(sergey.boldyrev@nokia.com).
-author(dmitry.kolesnikov@nokia.com).

%%
%% High order-function: http computation
%%

-export([
   % pipeline
   http/0,
   https/0,
   % hofs
   req/3,
   wait/2,
   recv/3,
   rsp/3
]).

%%
%% debug macro
-ifdef(DEBUG).
-define(DEBUG(M), error_logger:info_report([{?MODULE, self()}] ++ M)).
-else.
-define(DEBUG(M), true).
-endif.

%%-------------------------------------------------------------------
%%
%% Pipeline
%%
%%-------------------------------------------------------------------
http() ->
   emc:seq([
      fun
         ({http,_} = Uri) -> hof:ok(Uri);
         (_)             -> {error, uri_scheme}
      end,
      fun hof_inet:dns/2,
      fun hof_inet:tcp/2,
      fun hof_http:req/3,
      fun hof_http:wait/2,
      fun hof_http:recv/3,
      fun hof_http:rsp/3,
      fun hof_inet:statistic/1,
      fun hof_inet:close/1
   ]).

https() ->
   emc:seq([
      fun
         ({https,_} = Uri) -> hof:ok(Uri);
         (_)             -> {error, uri_scheme}
      end,
      fun hof_inet:dns/2,
      fun hof_inet:tcp/2,
      fun hof_inet:ssl/3,
      fun hof_http:req/3,
      fun hof_http:wait/2,
      fun hof_http:recv/3,
      fun hof_http:rsp/3,
      fun hof_inet:statistic/1,
      fun hof_inet:close/1
   ]).   

%%-------------------------------------------------------------------
%%
%% HOFs
%%
%%-------------------------------------------------------------------


%%
%% request HTTP service
%% 
req({Mod, Sock, _} = Sckt, {_,_} = Uri, Req) ->
   % method
   Method = case proplists:get_value(method, Req, get) of
      get  -> <<"GET">>;
      post -> <<"POST">>;
      head -> <<"HEAD">>
   end,
   % payload
   Payload = proplists:get_value(payload, Req, <<>>),
   % authority & path
   {Host, Path} = case proplists:get_value(proxy, Req) of
      undefined ->
         {
            list_to_binary(ek_uri:get(authority, Uri)),   
            list_to_binary(ek_uri:get(resource, Uri))
         };
      Proxy     ->
         {
            list_to_binary(ek_uri:get(authority, Proxy)), 
            list_to_binary(ek_uri:to_list(Uri))
         }
   end,
   % headers
   Head = case proplists:get_value(header, Req) of
      undefined -> <<>>;
      List      -> list_to_binary([<<H/binary, "\r\n">> || H <- List])
   end,
   Mod:send(Sock, <<
      Method/binary, " ", Path/binary, " HTTP/1.1\r\n",
      "Connection: close\r\n",
      "Accept: */*\r\n",
      "Host: ", Host/binary, "\r\n",
      Head/binary,
      "\r\n",
      Payload/binary
   >>),
   hof:ok(Sckt, Uri).
   

%%
%% wait service response 
%%
wait({Mod, Sock, _} = Sckt, Uri) ->
   case Mod:recv(Sock, 0) of
      {ok, Pckt}  -> 
         hof:ok(Sckt, Uri, Pckt);
      {error, _}  -> 
         hof:ok(Sckt, Uri, undefined)
   end.   

%%
%% receive service response 
%%
recv(Sock, Uri, undefined) ->
   hof:ok(Sock, Uri, undefined);
recv({Mod, Sock, _} = Sckt, Uri, Data) -> 
   case Mod:recv(Sock, 0) of
      {ok,   <<"0\r\n\r\n">>} ->
         % fix for chunked encoding
         hof:ok(Sckt, Uri, <<Data/binary, "0\r\n\r\n">>); 
      {ok,   Pckt}  ->
         recv(Sckt, Uri, <<Data/binary, Pckt/binary>>);
      {error, closed}  ->
         hof:ok(Sckt, Uri, Data)
   end.
      
   
%%
%% parse response as tuple
rsp(Sock, Uri, Data) ->
   hof:ok(
      Sock,
      ht_parse(Data, <<>>, undefined)
   ).

ht_parse(<<"\r\n", T/binary>>, Acc, undefined) ->
   {[_, Status, _], _} = lists:split(3, string:tokens(binary_to_list(Acc), " ")),
   Code = list_to_integer(Status),
   ht_parse(T, <<Acc/binary, "\r\n">>, Code);
   
ht_parse(<<"\r\n\r\n", T/binary>>, Acc, Code) ->
   {http, Code, <<Acc/binary, "\r\n\r\n">>, T};

ht_parse(<<H:8, T/binary>>, Acc, Code) ->
   ht_parse(T, <<Acc/binary, H:8>>, Code).

   
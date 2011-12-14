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
-include_lib("kernel/include/inet.hrl").

%%
%% High order-function: Internet
%%

-export([
   % pipeline
   dns/0,
   tcp/0,
   ssl/0,
   % hofs
   dns/1,
   dns/2,
   tcp/1,
   tcp/2,
   ssl/3,
   statistic/1,
   close/1
]).

%%
%% debug macro
-ifdef(DEBUG).
-define(DEBUG(M), error_logger:info_report([{?MODULE, self()}] ++ M)).
-else.
-define(DEBUG(M), true).
-endif.

%%
%% Socket options
-define(TCP_OPTS, [binary, {packet, 0}, {active, false}]).
-define(SSL_OPTS, [{reuse_sessions, false}]).

%%-------------------------------------------------------------------
%%
%% Pipeline
%%
%%-------------------------------------------------------------------
dns() ->
   emc:seq([
      fun
         ({dns,_} = Uri) -> hof:ok(Uri);
         (_)             -> {error, uri_scheme}
      end,
      fun hof_inet:dns/2,
      fun() -> hof:ok([]) end %% fixes pipiline dimension & injects empty statistic
   ]).

tcp() ->
   emc:seq([
      fun
         ({tcp,_} = Uri) -> hof:ok(Uri);
         (_)             -> {error, uri_scheme}
      end,
      fun hof_inet:dns/2,
      fun hof_inet:tcp/2,
      fun hof_inet:close/1,
      fun() -> hof:ok([]) end %% fixes pipiline dimension & injects empty statistic
   ]).

ssl() ->
   emc:seq([
      fun
         ({ssl,_} = Uri) -> hof:ok(Uri);
         (_)             -> {error, uri_scheme}
      end,
      fun hof_inet:dns/2,
      fun hof_inet:tcp/2,
      fun hof_inet:ssl/3,
      fun hof_inet:statistic/1,
      fun hof_inet:close/1
   ]).
   
%%-------------------------------------------------------------------
%%
%% HOFs
%%
%%-------------------------------------------------------------------

%%
%% resolve IP address of URI authority
dns(Uri) ->
   dns(Uri, []).
dns(Uri, undefined) ->
   dns(Uri, []);
dns({dns, _} = Uri, Req)  ->   
   Host = ek_uri:get(host, Uri),
   IPvX = case proplists:is_defined(inet6, Req) of 
      true  -> inet6;
      false -> inet
   end,
   case inet:gethostbyname(Host, IPvX) of
      {ok,  DNS} -> 
         ?DEBUG([{dns, DNS}]),
         hof:ok({dns,  0, format(dns, DNS), <<>>});
      {error, E} -> 
         ?DEBUG([{dns, {error, E}}]),
         hof:ok({dns, -1, <<>>, <<>>})
   end;   
dns(Uri, Req)  ->
   Host = ek_uri:get(host, Uri),
   IPvX = case proplists:is_defined(inet6, Req) of 
      true  -> inet6;
      false -> inet
   end,
   {ok, Addr} = inet:getaddr(Host, IPvX),
   hof:ok(
      Uri,
      [{ip, Addr} | Req]
   ).

%%
%% Establish TCP/IP connection to URI authority
tcp(Uri) ->
   tcp(Uri, []).
tcp(Uri, undefined) ->
   tcp(Uri, []);
tcp({tcp,_} = Uri, Req) ->
   Host = proplists:get_value(ip, Req, ek_uri:get(host, Uri)),
   Port = ek_uri:get(port, Uri),
   case gen_tcp:connect(Host, Port, ?TCP_OPTS) of
      {ok, Tcp} ->
         hof:ok(
            {gen_tcp, Tcp, Tcp},
            {tcp, 0, format(tcp, Tcp), <<>>}
         );
      {error, E} ->
         ?DEBUG([{tcp, {error, E}}]),
         hof:ok(
            undefined,
            {tcp, -1, <<>>, <<>>}
         )
   end;
tcp({_,_} = Uri, Req) ->
   {Host, Port} = case proplists:get_value(proxy, Req) of
      undefined ->
         {ek_uri:get(host, Uri), ek_uri:get(port, Uri)};
      Proxy     ->
         {ek_uri:get(host, Proxy), ek_uri:get(port, Proxy)}
   end,
   {ok, Tcp} = gen_tcp:connect(Host, Port, ?TCP_OPTS),
   hof:ok(
      {gen_tcp, Tcp, Tcp},  %% TODO: triple preserve TCP/IP socket for statistic
      Uri,
      Req
   ).
   
%%
%% Establish SSL connection to URI authority
ssl({gen_tcp, Sock, Tcp}, {ssl, _}, Req) ->
   case ssl:connect(Sock, ?SSL_OPTS) of
      {ok,  Ssl} ->
         {ok, Cert} = ssl:peercert(Ssl),
         hof:ok(
            {ssl, Ssl, Tcp},
            {ssl, 0, format(ssl,Ssl), <<>>}
         );
      {error, E} ->
         ?DEBUG([{ssl, {error, E}}]),
         hof:ok(
            undefined,
            {ssl, -1, <<>>, <<>>}
         )
   end;

ssl({gen_tcp, Sock, Tcp}, {_,_}=Uri, Req) ->   
   {ok, Ssl}  = ssl:connect(Sock, ?SSL_OPTS),
   {ok, Cert} = ssl:peercert(Ssl),
   hof:ok(
      {ssl, Ssl, Tcp},
      Uri,
      Req
   ).
     
  
%%
%% Reports statistic on socket usage
statistic(undefined) ->
   hof:ok(undefined, []);
statistic({_, _, Tcp} = Sock) ->
   case inet:getstat(Tcp, [recv_avg, recv_cnt, recv_oct]) of
      {ok, Stat} ->
         hof:ok(Sock, Stat);
      _          ->
         hof:ok(Sock, [])
   end.

%%
%%
close(undefined) ->
   hof:ok();
close({Mod, Sock, _}) ->
   Mod:close(Sock),
   hof:ok().

   
%%-------------------------------------------------------------------
%%
%% Private
%%
%%-------------------------------------------------------------------

%%
%% Pretty format of service output
format(dns, DNS) ->
   Aliases = lists:map(
      fun(X) -> "\r\n\t" ++ X end, 
      DNS#hostent.h_aliases
   ),
   Addrs   = lists:map(
      fun(X) -> "\r\n\t" ++ ip_to_list(X) end, 
      DNS#hostent.h_addr_list
   ),
   list_to_binary(
      io_lib:fwrite(
         "Host:\r\n\t~s\r\nAliases:~s\r\nAddresses:~s\r\n", 
         [DNS#hostent.h_name, Aliases, Addrs]
      )
   );
   
format(tcp, Sock) ->
   % TODO: tcp/ip report
   <<>>;

format(ssl, Sock) ->
   {ok, Cert} = ssl:peercert(Sock),
   CA = public_key:pkix_decode_cert(Cert, otp),
   % TODO: ssl report
   list_to_binary(
      lists:flatten(io_lib:print(CA))
   ).
   
ip_to_list({N1,N2,N3,N4}) ->
   integer_to_list(N1) ++ "." ++
   integer_to_list(N2) ++ "." ++
   integer_to_list(N3) ++ "." ++
   integer_to_list(N4);
   
ip_to_list({N1,N2,N3,N4,N5,N6,N7,N8}) ->   
   integer_to_list(N1) ++ ":" ++
   integer_to_list(N2) ++ ":" ++
   integer_to_list(N3) ++ ":" ++
   integer_to_list(N4) ++ ":" ++
   integer_to_list(N5) ++ ":" ++
   integer_to_list(N6) ++ ":" ++
   integer_to_list(N7) ++ ":" ++
   integer_to_list(N8).

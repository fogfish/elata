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
-module(fe_web).
-author(dmitry.kolesnikov@nokia.com).

%%
%% API:
%%
%% http://{host}:{port}/config/agent 
%%   GET
%%   List of agents configured within the system
%%   {
%%      id: 
%%      title:
%%   }
%%
%% http://{be}/{user}   <- GET
%% http://{be}/profile  <- POST
%%
%%   User profile & list of use-cases
%%   {
%%      username:
%%      usermail:
%%      usecase: [{
%%                  id:
%%                  title:
%%                }]
%%   }
%%


-export([
   h/1
]).

%%
%% dispatch loop
h(Req) ->
   try
      h(Req:get(method), string:tokens(Req:get(path), "/"), Req)
   catch
      Klass:Err ->
         Req:respond({500, [{"Conten-Type", "text/plain"}], io_lib:write([{Klass, Err}, erlang:get_stacktrace()])})
   end.
   
%%%------------------------------------------------------------------
%%%
%%% Web category
%%%
%%%------------------------------------------------------------------
h('GET', [], Req) ->
   {ok, Val} = kvs:get("kvs:/elata/web", "index.html"),
   Req:respond({200, [{"Conten-Type", "text/html"}], Val});

h('GET', ["js", File], Req) ->
   {ok, Val} = kvs:get("kvs:/elata/web", "js/" ++ File),
   Req:respond({200, [{"Conten-Type", "application/javascript"}], Val});

h('GET', ["style", File], Req) ->
   {ok, Val} = kvs:get("kvs:/elata/web", "style/" ++ File),
   case filename:extension(File) of
      ".png" -> 
         Req:respond({200, [{"Conten-Type", "image/png"}], Val});
      _      ->   
         Req:respond({200, [{"Conten-Type", "text/css"}], Val})
   end;
   
%%%------------------------------------------------------------------
%%%
%%% User category
%%%
%%%------------------------------------------------------------------
h('GET', ["config", "agent"], Req) ->
   Nodes = lists:map(
      fun(N) ->
         elata_to_mochi([
            {id,    ek_uri:authority(N)},
            {title, ek_uri:authority(N)}
         ])
      end,
      ek:nodes()
   ),
   Req:respond({
      200, 
      [{"Conten-Type", "application/json"}], 
      list_to_json(Nodes)
   });
   
   
%%%------------------------------------------------------------------
%%%
%%% User category
%%%
%%%------------------------------------------------------------------
h('POST', ["user"], Req) ->
   User  = json_to_elata(Req:recv_body(), [<<"username">>, <<"usermail">>]),
   % user identity is uri http://{be}/user
   Uid  = uid(proplists:get_value(username,  User)), 
   case kvs:get("kvs:/elata/user", Uid) of
      {error, not_found} ->
         ok    = kvs:put("kvs:/elata/user", Uid, User),
         Req:respond({
            200, 
            [{"Conten-Type", "application/json"}], 
            elata_to_json([{usecase, elata_to_mochi([])} | User])
         });
      {ok, User} ->
         {ok, Cases} = kvs:get("kvs:/elata/usecase", Uid, []),
         Req:respond({
            200, 
            [{"Conten-Type", "application/json"}], 
            elata_to_json([{usecase, elata_to_mochi(Cases)} | User])
         })
   end;

h('GET', [User],   Req) ->
   Uid         = uid(User),
   {ok, UObj}  = kvs:get("kvs:/elata/user",    Uid),
   {ok, Cases} = kvs:get("kvs:/elata/usecase", Uid, []),
   Req:respond({
      200, 
      [{"Conten-Type", "application/json"}], 
      elata_to_json([{usecase, elata_to_mochi(Cases)} | UObj])
   });
   
%%%------------------------------------------------------------------
%%%
%%% Process category
%%%
%%%------------------------------------------------------------------

%% /user/process -> process id
h('POST', [User, "process"], Req) ->
   %% pre-process input
   Val  = json_to_elata(Req:recv_body(), [
      <<"usecase">>, <<"script">>, <<"http">>, <<"proxy">>, <<"thinktime">>,
      <<"ttl">>
   ]),
   Right = case be_process:put(proplists:delete(usecase, Val)) of
      %% new process user is owner
      {ok, Pid}                      -> 7; %xrw
      %% existed process user is observer
      {error, {already_exists, Pid}} -> 6  %xr-
   end,
   Uid  = uid(User),
   Case =  {ek_uri:to_binary(Pid),  
      {struct, [
         {<<"title">>, proplists:get_value(usecase, Val)},
         {<<"right">>, Right}
      ]}
   },
   {ok, Cases} = kvs:get("kvs:/elata/usecase", Uid, []),
   ok = kvs:put("kvs:/elata/usecase", Uid, [Case | Cases]),
   Req:respond({200, [{"Conten-Type", "application/json"}], "\"" ++ ek_uri:to_list(Pid) ++ "\""});
   
h('GET', [User, Schema, Key], Req) ->
   K   =  list_to_binary(Key),
   Pid = {list_to_atom(Schema), ek:node(), <<"/", K/binary>>},  
   {ok, Val} = kvs:get("kvs:/elata/proc", Pid),
   List = lists:map(
      fun(Node) ->
         Tid  = {http, ek_uri:authority(Node), <<"/", K/binary, "/uri">>},   
         {ok, T} = kvs:get("kvs:/elata/ds", Tid, 0),
         Bauth   = ek_uri:authority(Node),
         Icon    = <<"/view/", Bauth/binary, "/", K/binary, "/icon.image/1hours.png">>, 
         {Bauth, 
            {struct, [
               {<<"latency">>, T}, 
               {<<"icon">>,    Icon}
            ]}
         }
      end,
      ek:nodes()
   ),
   MVal = [{id, ek_uri:to_binary(Pid)}, {telemetry, elata_to_mochi(List)} | Val],
   Req:respond({
      200, 
      [{"Conten-Type", "application/json"}], 
      elata_to_json(MVal)
   });
   
h('DELETE', [User, Schema, Key], Req) ->
   K   =  list_to_binary(Key),
   Pid = {list_to_atom(Schema), ek:node(), <<"/", K/binary>>},
   Uid  = uid(User),

   Pid        = hex_to_bin(Key),
   {ok, Proc} = kvs:get("kvs:/elata/proc", Pid),
   Uid        = ek_uri:to_binary(uid(User)),
   Owner      = proplists:get_value(owner, Proc),
   % delete process 
   if
      Uid =:= Owner ->
         ok = kvs:remove("kvs:/elata/proc", Pid),
         % drop rendering
         lists:foreach(
           fun(Node) ->
              lists:foreach(
                 fun(Template) ->
                    ok = kvs:remove("kvs:/elata/view", vid(Node, Pid, Template))
                 end,
                 [<<"uri.image">>, <<"latency.image">>, <<"icon.image">>, <<"tcp.image">>, <<"http.image">>, <<"availability.image">>]
              )
           end,
           ek:nodes()
         );
      true ->
         ok
   end,
   % update user record
   {ok, Cases} = kvs:get("kvs:/elata/usecase", uid(User)),
   ok = kvs:put("kvs:/elata/usecase", uid(User), 
      lists:foldl(
         fun({Id,_}=Obj, Acc) ->
            Bid = hex_to_bin(binary_to_list(Id)),
            if
               Pid =/=  Bid -> Acc ++ [Obj];
               true         -> Acc
            end
         end,
         [],
         Cases
      )
   ),
   Req:respond({200, [], "\"" ++ Key ++ "\""});
   
%%%------------------------------------------------------------------
%%%
%%% Telemetry category
%%%
%%%------------------------------------------------------------------

% /user/telemetry/{key}
h('GET', [User, "telemetry", Key], Req) ->   
   BKey = list_to_binary(Key),
   List = lists:map(
      fun(Node) ->                                                                                        
         {ok, Rsp}  = kvs:get("kvs:/elata/rsp", {http, ek_uri:authority(Node), <<"/", BKey/binary>>}, <<"">>),
         {ok, Doc}  = kvs:get("kvs:/elata/doc", {http, ek_uri:authority(Node), <<"/", BKey/binary>>}, <<"">>), 
         {ok, Dns}  = kvs:get("kvs:/elata/ds",  {http, ek_uri:authority(Node), <<"/", BKey/binary, "/dns">>},  0),
         {ok, Tcp}  = kvs:get("kvs:/elata/ds",  {http, ek_uri:authority(Node), <<"/", BKey/binary, "/tcp">>},  0),
         {ok, Ssl}  = kvs:get("kvs:/elata/ds",  {http, ek_uri:authority(Node), <<"/", BKey/binary, "/ssl">>},  0),
         {ok, Ttfb} = kvs:get("kvs:/elata/ds",  {http, ek_uri:authority(Node), <<"/", BKey/binary, "/ttfb">>}, 0),
         {ok, Ttmr} = kvs:get("kvs:/elata/ds",  {http, ek_uri:authority(Node), <<"/", BKey/binary, "/ttmr">>}, 0),
         {ok, RecvAvg} = kvs:get("kvs:/elata/ds",  {http, ek_uri:authority(Node), <<"/", BKey/binary, "/recv_avg">>}, 0),
         {ok, RecvCnt} = kvs:get("kvs:/elata/ds",  {http, ek_uri:authority(Node), <<"/", BKey/binary, "/recv_cnt">>}, 0),
         {ok, RecvOct} = kvs:get("kvs:/elata/ds",  {http, ek_uri:authority(Node), <<"/", BKey/binary, "/recv_oct">>}, 0),
         {ok, Lat}  = kvs:get("kvs:/elata/ds",  {http, ek_uri:authority(Node), <<"/", BKey/binary, "/uri">>},  0),
         try
            mochijson2:encode(Doc),
            {ek_uri:authority(Node), elata_to_mochi([{doc, <<Rsp/binary, Doc/binary>>}, {dns, Dns}, {tcp, Tcp}, {ssl, Ssl}, {ttfb, Ttfb}, {ttmr, Ttmr}, {recv_avg, RecvAvg}, {recv_cnt, RecvCnt}, {recv_oct, RecvOct}, {latency, Lat}])}
         catch
            _:_ ->
            SafeDoc = base64:encode(Doc),
            {ek_uri:authority(Node), elata_to_mochi([{doc, <<Rsp/binary, SafeDoc/binary>>}, {dns, Dns}, {tcp, Tcp}, {ssl, Ssl}, {ttfb, Ttfb}, {ttmr, Ttmr}, {recv_avg, RecvAvg}, {recv_cnt, RecvCnt}, {recv_oct, RecvOct}, {latency, Lat}])}
         end
      end,
      ek:nodes()
   ),
   Req:respond({
      200, 
      [{"Conten-Type", "application/json"}], 
      elata_to_json(List)
   });

%%%------------------------------------------------------------------
%%%
%%% statistic
%%%
%%%------------------------------------------------------------------
h('GET', ["view", Node, Key, View, Scale], Req) ->
   Uri  = ek_uri:new("http://" ++ Node ++ "/" ++ Key),
   File = ek_uri:lhash(authority, Uri) ++ "/" ++ Key ++ "/" ++ View ++ "/" ++ Scale ++ ".png",
   {ok, Val} = kvs:get("kvs:/elata/img", File),
   Req:respond({
      200,
      [{"Conten-Type", "image/png"}],
      Val
   }).



%%%------------------------------------------------------------------
%%%
%%% Private
%%%
%%%------------------------------------------------------------------

%%
%% create user identity
uid(User) when is_binary(User) ->
   {http, ek:node(), <<"/", User/binary>>};
uid(User) when is_list(User)->
   {http, ek:node(), list_to_binary("/" ++ User)}.

%%
%% create view identity
vid(Node, Key, Template) ->
   K = list_to_binary(bin_to_hex(Key)),
   {http, ek_uri:authority(Node), <<"/", K/binary, "#", Template/binary>>}.


%%   
%% binary to hex
bin_to_hex(Bin) ->
   bin_to_hex(Bin, "").
   
bin_to_hex(<<>>, Acc) ->
   Acc;
bin_to_hex(<<X:8, T/binary>>, Acc) ->  
   bin_to_hex(T, Acc ++ [to_hex(X div 16), to_hex(X rem 16)]).
   
to_hex(X) when X < 10 ->
   $0 + X;
to_hex(X) ->
   $a + (X - 10).

%%
%% hex to binary
hex_to_bin(Hex) ->
   hex_to_bin(Hex, <<>>).

hex_to_bin([], Acc) ->
   Acc;
hex_to_bin([H, L | T], Acc) ->
   V = to_int(H) * 16 + to_int(L),
   hex_to_bin(T, <<Acc/binary, V>>).

to_int(C) when C >= $a, C =< $f ->
   10 + (C - $a);
to_int(C) when C >= $0, C =< $9 ->
   C - $0.
   
   
%%
%% translates basic ELATA obj into mochiweb Json suitabe format
elata_to_mochi(Val) when is_list(Val) ->
   MVal = lists:map(
      fun
         ({K,V}) when is_binary(K) -> {K,     V};
         ({K,V}) -> {atom_to_binary(K, utf8), V}
      end,
      Val
   ),
   {struct, MVal}.

   
%%
%% Translates JSON into Elata format
json_to_elata(Json, Attrs) ->
   {struct, Mochi} = mochijson2:decode(Json),
   lists:map(
      fun({K, V}) ->
         true = lists:member(K, Attrs),
         {binary_to_atom(K, utf8), V}
      end,
      Mochi
   ).

elata_to_json(Val) ->
   Json = mochijson2:encode(elata_to_mochi(Val)),
   iolist_to_binary(Json).
   
list_to_json(Val) ->
   Json = mochijson2:encode(Val),
   iolist_to_binary(Json).
   

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


-export([
   h/1
]).

%%
%% available telemetry
-define(TM, [
   <<"uri">>,
   <<"dns">>,
   <<"tcp">>,
   <<"ssl">>,
   <<"ttfb">>,
   <<"ttmr">>,
   <<"code">>,
   <<"pckt">>,
   <<"size">>,
   <<"chnk">>
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

h('GET', ["image" | T], Req) ->
   File = lists:foldl(
      fun(X, Acc) -> Acc ++ "/" ++ X end,
      "",
      T
   ),
   {ok, Val} = kvs:get("kvs:/elata/img", File),
   Req:respond({
      200,
      [{"Conten-Type", "image/png"}],
      Val
   });   
   
h('GET', ["cluster"], Req) ->
   be_web:h(Req);
   
%%%------------------------------------------------------------------
%%%
%%% User category
%%%
%%%------------------------------------------------------------------
h('POST', ["user"], Req) ->
   {struct, Json} = mochijson2:decode(Req:recv_body()),
   Uid = uid(proplists:get_value(<<"username">>, Json)),
   {ok, User} = case kvs:get("kvs:/elata/user", Uid) of
      {ok, {struct, U}}  -> 
         case lists:member({usermail, proplists:get_value(<<"usermail">>, Json)}, U) of
            true  -> {ok, {struct, U}};
            false -> {error, noaccess}
         end;
      {error, not_found} ->
         U = {struct, [
            {username, proplists:get_value(<<"username">>, Json)},
            {usermail, proplists:get_value(<<"usermail">>, Json)},
            {usecases, []}
         ]},
         ok = kvs:put("kvs:/elata/user", Uid, U),
         {ok, U}
   end,
   Req:respond({
      200, 
      [{"Conten-Type", "application/json"}], 
      mochijson2:encode(User)
   });

h('GET', [Username],   Req) ->
   Uid         = uid(Username),
   {ok, User}  = kvs:get("kvs:/elata/user",    Uid),
   Req:respond({
      200, 
      [{"Conten-Type", "application/json"}], 
      mochijson2:encode(User)
   });
   
%%%------------------------------------------------------------------
%%%
%%% Process category
%%%
%%%------------------------------------------------------------------

%% /user/process -> process id
h('POST', [Username, "process"], Req) ->
   {struct, Json} = mochijson2:decode(Req:recv_body()),
   Uid  = uid(Username),
   {ok, {struct, User}} = kvs:get("kvs:/elata/user", Uid),
   Proc = [
      {script,    proplists:get_value(<<"script">>,    Json)},
      {http,      proplists:get_value(<<"http">>,      Json)},
      {thinktime, proplists:get_value(<<"thinktime">>, Json)}
   ],
   {Pid, Right} = case be_process:put(Proc) of
      %% new process user is owner
      {ok, Pid}                      -> {Pid, 7}; %xrw
      %% existed process user is observer
      {error, {already_exists, Pid}} -> {Pid, 6}  %xr-
   end,
   Case = {struct, [
      {service,  proplists:get_value(<<"service">>,    Json)},
      {usecase,  proplists:get_value(<<"usecase">>,    Json)},
      {pid,      ek_uri:to_binary(Pid)},
      {right,    Right}
   ]},
   UCases = [Case | proplists:get_value(usecases, User)],
   NUser  = [{usecases, UCases} | proplists:delete(usecases, User)],
   ok = kvs:put("kvs:/elata/user", Uid, {struct, NUser}),
   Req:respond({200, [{"Conten-Type", "application/json"}], "\"" ++ ek_uri:to_list(Pid) ++ "\""});
   
h('GET', [Username, "process" | _], Req) ->
   {_,  Pid}  = lists:split(length(Username) + 7 + 3, Req:get(path)),
   {ok, Proc} = kvs:get("kvs:/elata/proc", ek_uri:new(Pid)),
   TM = lists:map(
      fun(Node) ->
         Tid = ek_uri:append(path, "/uri", 
            ek_uri:set(authority, ek_uri:get(authority, Node), Pid)
         ),
         Icon = list_to_binary(
            ek_uri:to_path(
               ek_uri:append(path, "/icon.image/1hours.png", 
                  ek_uri:set(authority, ek_uri:get(authority, Node), Pid)
               )
            )
         ),
         {ok, Ltnc} = kvs:get("kvs:/elata/ds", Tid, 0),
         {ek_uri:get(authority, Node), 
            {struct, [
               {<<"uri">>, Ltnc},
               {<<"icon.image">>,  <<"/image", Icon/binary>>}
            ]}
         }
      end,
      ek:nodes()
   ),
   R = lists_to_mjson([
      {id, ek_uri:to_binary(Pid)},
      {tm, {struct, TM}} 
      | Proc   
   ]),
   Req:respond({
      200, 
      [{"Content-Type", "application/json"}], 
      mochijson2:encode(R)
   });
   
h('GET', [Username, "telemetry" | _], Req) ->   
   {_,  Pid}  = lists:split(length(Username) + 9 + 3, Req:get(path)),  
   {ok, Proc} = kvs:get("kvs:/elata/proc", ek_uri:new(Pid)),
   TM = lists:map(
      fun(Node) ->
         L1 = lists:map(
            fun(Sfx) ->
               Tid = ek_uri:append(path, "/" ++ binary_to_list(Sfx), 
                  ek_uri:set(authority, ek_uri:get(authority, Node), Pid)
               ),
               {ok, Val} = kvs:get("kvs:/elata/ds", Tid, 0),
               {Sfx, Val}
            end,
            ?TM
         ),
         L2 = lists:map(
            fun(View) ->
               Icon = list_to_binary(ek_uri:to_path(
                  ek_uri:append(path, "/" ++ binary_to_list(View), 
                     ek_uri:set(authority, ek_uri:get(authority, Node), Pid)
                  )
               )),
               {View, <<"/image", Icon/binary>>}
            end,
            proplists:get_value(view, Proc)
         ),
         {ok, Rsp} = kvs:get("kvs:/elata/rsp", ek_uri:set(authority, ek_uri:get(authority, Node), Pid), <<"">>),
         {ok, Doc} = kvs:get("kvs:/elata/doc", ek_uri:set(authority, ek_uri:get(authority, Node), Pid), <<"">>),
         L3 = try 
            mochijson2:encode(Doc),
            [{<<"response">>, Rsp}, {<<"payload">>, Doc}]
         catch
         _:_ ->
            SDoc = base64:encode(Doc),
            [{<<"response">>, Rsp}, {<<"payload">>, SDoc}]
         end,
         {ek_uri:get(authority, Node), 
            {struct, L1 ++ L2 ++ L3}
         }
      end,
      ek:nodes()
   ),
   R = lists_to_mjson([
      {id, ek_uri:to_binary(Pid)},
      {tm, {struct, TM}} 
      | Proc
   ]),
   Req:respond({
      200, 
      [{"Content-Type", "application/json"}], 
      mochijson2:encode(R)
   });
   
   
   
h('DELETE', [Username, "process" | _], Req) ->
   {_,  Pid}  = lists:split(length(Username) + 7 + 3, Req:get(path)),
   {ok, Proc} = kvs:get("kvs:/elata/proc", ek_uri:new(Pid)),
   Uid = uid(Username),
   {ok, {struct, User}} = kvs:get("kvs:/elata/user", Uid),
   %% update user record
   {Case, UCases} = lists:foldl(
      fun({struct, U}, {C, UC}) ->
         case lists:member({pid, ek_uri:to_binary(Pid)}, U) of 
            true  -> {U, UC};
            false -> {C, [{struct, U} | UC]}
         end
      end,
      {undefined, []},
      proplists:get_value(usecases, User)
   ),
   % delete process if user is owner
   case lists:member({right, 7}, Case) of
      true  -> be_process:remove(Pid);
      false -> ok
   end,
   % update user
   NUser  = [{usecases, UCases} | proplists:delete(usecases, User)],
   ok = kvs:put("kvs:/elata/user", Uid, {struct, NUser}),
   Req:respond({200, [], "\"" ++ ek_uri:to_list(Pid) ++ "\""}).
   

%%%------------------------------------------------------------------
%%%
%%% Private
%%%
%%%------------------------------------------------------------------

%%
%% create user identity
uid(User) when is_binary(User) ->
   uid(binary_to_list(User));
uid(User) ->
   ek_uri:set(path, "/" ++ User,
      ek_uri:set(authority, ek:node(), ek_uri:new(http))
   ).

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
   
   
   
lists_to_mjson(List) -> 
   {struct,
      lists:map(
         fun
            ({K,V}) when is_binary(K) -> {K,     V};
            ({K,V}) -> {atom_to_binary(K, utf8), V}
         end,
         List
      )
   }.   
   

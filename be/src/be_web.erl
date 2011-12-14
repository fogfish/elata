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
-module(be_web).
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
   {ok, Val} = kvs:get("kvs:/elata/console", "index.html"),
   Req:respond({200, [{"Conten-Type", "text/html"}], Val});

h('GET', ["js", File], Req) ->
   {ok, Val} = kvs:get("kvs:/elata/console", "js/" ++ File),
   Req:respond({200, [{"Conten-Type", "application/javascript"}], Val});

h('GET', ["style", File], Req) ->
   {ok, Val} = kvs:get("kvs:/elata/console", "style/" ++ File),
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
   {ok, Val} = kvs:get("kvs:/elata/g", File),
   Req:respond({
      200,
      [{"Conten-Type", "image/png"}],
      Val
   });
   
h('GET', ["cluster"], Req) ->
   Nodes = lists:map(
      fun(N) ->
         {ok, Info} = ek_prot:node_info(ek:whereis(N)),
         {struct, [
            {<<"id">>,    list_to_binary(ek_uri:get(authority, N))},
            {<<"title">>, list_to_binary(proplists:get_value(name, Info))}
         ]}
      end,
      ek:nodes()
   ),
   Req:respond({
      200, 
      [{"Content-Type", "application/json"}], 
      mochijson2:encode(Nodes)
   });   
   
%%%------------------------------------------------------------------
%%%
%%% Process category
%%%
%%%------------------------------------------------------------------   
h('GET', ["process"], Req) ->
   R = kvs:map(
      "kvs:/elata/proc",
      fun(Pid, Proc) ->
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
         lists_to_mjson([
            {id, ek_uri:to_binary(Pid)},
            {tm, {struct, TM}} 
            | Proc
         ])
      end
   ),
   Req:respond({
      200, 
      [{"Content-Type", "application/json"}], 
      mochijson2:encode(R)
   });
   
h('GET', ["process" | _], Req) ->
   {_, Pid}   = lists:split(9, Req:get(path)), 
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
   
h('POST', ["process"], Req) ->
   {struct, Json} = mochijson2:decode(Req:recv_body()),
   Proc = [
      {script,    proplists:get_value(<<"script">>,    Json)},
      {http,      proplists:get_value(<<"http">>,      Json)},
      {thinktime, proplists:get_value(<<"thinktime">>, Json)}
   ],
   {ok, Pid} = be_process:put(Proc),
   Req:respond({
      200, 
      [{"Content-Type", "application/json"}], 
      "\"" ++ ek_uri:to_list(Pid) ++ "\""
   });
   
h('DELETE', ["process" | _], Req) ->
   {_, Pid}   = lists:split(9, Req:get(path)),
   be_process:remove(Pid),
   Req:respond({
      200, 
      [{"Content-Type", "application/json"}], 
      "\"" ++ ek_uri:to_list(Pid) ++ "\""
   }).
   
%%%------------------------------------------------------------------
%%%
%%% Private
%%%
%%%------------------------------------------------------------------   
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

   
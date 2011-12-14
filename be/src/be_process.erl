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
-module(be_process).
-author(dmitry.kolesnikov@nokia.com).

%%
%% Elata: process management
%%

%%
%% Process attributes
%%  script
%%  http
%%  proxy
%%  thinktime
%%  ttl
%%  view


-export([
   put/1,
   remove/1,
   spawn_views/3   
]).


%%
%% put(Spec) -> ok
%%
put(Spec) ->
   Pid   = pid(Spec),
   NSpec = set_default_views(Spec),
   case kvs:has("kvs:/elata/proc", Pid) of
      false -> 
         kvs:put("kvs:/elata/proc", Pid, NSpec),
         spawn_views(ek:nodes(), Pid, NSpec),
         {ok, Pid};
      true  ->
         {error, {already_exists, Pid}}
   end.

%%
%% remove(Pid)
%%
remove(Id) ->
   Pid = ek_uri:new(Id),
   {ok, Spec} = kvs:get("kvs:/elata/proc", Pid),
   kvs:remove("kvs:/elata/proc", Pid),
   Views    = proplists:get_value(view, Spec, []),
   lists:foreach(
      fun(Node) ->
         lists:foreach(
            fun(Tpl) ->
               kvs:remove(
                  "kvs:/elata/view", 
                  ek_uri:append(path, "/" ++ binary_to_list(Tpl),
                     ek_uri:set(authority, ek_uri:get(authority, Node), Pid)
                  )
               )
            end,
            Views
         )
      end,
      ek:nodes()
   ).

%%
%%
spawn_views(Nodes, Pid, Spec) ->
   Key      = ek_uri:get(path, Pid),
   Views    = proplists:get_value(view, Spec, []),
   lists:foreach(
      fun(Node) ->
         lists:foreach(
            fun(Tpl) ->
               ok = kvs:put(
                  "kvs:/elata/view", 
                  ek_uri:append(path, "/" ++ binary_to_list(Tpl),
                     ek_uri:set(authority, ek_uri:get(authority, Node), Pid)
                  ),
                  Spec
               )
            end,
            Views
         )
      end,
      Nodes
   ).
   
%%-------------------------------------------------------------------   
%%
%% Private
%%
%%-------------------------------------------------------------------   

%%
%% calculates process identity
pid(Spec) ->
   Uri  = proplists:get_value(script, Spec),
   Hash = crypto:sha(
      term_to_binary([Uri, proplists:get_value(http, Spec)])
   ),
   Key   = hof:bin_to_hex(Hash),
   ek_uri:set(path, "/" ++ Key, 
      ek_uri:set(authority, ek:node(), 
         ek_uri:set(schema, ek_uri:get(schema, Uri), 
            ek_uri:new()
         )
      )
   ).


%%
%% set default views for process
set_default_views(Spec) ->
   Uri      = proplists:get_value(script, Spec),
   Views    = proplists:get_value(view,   Spec, []),
   Default  = [
      <<"icon.image">>, 
      <<"latency.image">>, 
      <<"dns.image">>, 
      <<"error.image">>
   ],
   Schema   = case ek_uri:get(schema, Uri) of
      tcp  ->
         [
         <<"uri-tcp.image">>,   
         <<"tcp.image">>, 
         <<"error.image">>
         ];
      ssl  -> 
         [
         <<"uri-ssl.image">>,
         <<"tcp.image">>,
         <<"ssl.image">>,
         <<"size.image">>,
         <<"pckt.image">>,
         <<"pckt-cnt.image">>
         ];
      http -> 
         [
         <<"uri-http.image">>,
         <<"tcp.image">>,
         <<"ttfb.image">>,
         <<"ttmr.image">>,
         <<"error.image">>,
         <<"size.image">>,
         <<"pckt.image">>,
         <<"pckt-cnt.image">>
         ];
      https-> 
         [
         <<"uri-https.image">>,
         <<"tcp.image">>,
         <<"ssl.image">>,
         <<"ttfb.image">>, <<"ttmr.image">>, <<"error.image">>, <<"size.image">>, <<"pckt.image">>, <<"pckt-cnt.image">>];
      _    -> []
   end,
   NViews   = lists:merge(Views, Default ++ Schema),
   [{view, NViews} | proplists:delete(view, Spec)].   
   

   
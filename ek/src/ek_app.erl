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
-module(ek_app).
-behaviour(application).
-author(dmitry.kolesnikov@nokia.com).

-export([
   start/2,
   stop/1
]).

-define(APPNAME,  elata_ek).
-define(NODE,     "ws://localhost:8080"). 

start(_Type, Args) -> 
   % Config
   Config = config(?APPNAME, Args, [{node, ?NODE}, proxy]),
   case ek_sup:start_link(Config) of
      {ok, Pid} ->
         Node = proplists:get_value(node, Config),
         Uri  = ek_uri:new(Node),
         Port = proplists:get_value(port, Uri),
         ek_ws_sup:listen(Port),
         ets:new(ek_nodes, [public, named_table]),
         % register itself 
         % TODO: automatic node discovery
         ets:insert(ek_nodes, {self, Node}),
         {ok,Pid};
      Other     -> {error, Other}
   end. 

stop(_State) ->
        ok.

        
%%%------------------------------------------------------------------
%%%
%%%  Private 
%%%
%%%------------------------------------------------------------------   
config(App, DList, List) ->
   config(App, DList, List, []).

config(App, DList, [{Key, Default} | T], Acc) ->
   AppCfgVal = case application:get_env(App, Key) of 
      undefined   -> Default;
      {ok, Value} -> Value
   end,
   Val = proplists:get_value(Key, DList, AppCfgVal),
   config(App, DList, T, [{Key, Val} | Acc]);

config(App, DList, [Key | T], Acc) ->
   AppCfgVal = case application:get_env(App, Key) of 
      undefined   -> undefined;
      {ok, Value} -> Value
   end,
   Val = proplists:get_value(Key, DList, AppCfgVal),
   case Val of 
      undefined   -> config(App, T, Acc);
      _           -> config(App, T, [{Key, Val} | Acc])
   end;
   
config(_App, DList, [], Acc) ->
   Acc.
   

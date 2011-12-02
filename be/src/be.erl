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
-module(be).
-author(dmitry.kolesnikov@nokia.com).

-export([
   start/0,
   spawn/2
]).

%%
%%
start() ->
   % load application resource
   {file, Module} = code:is_loaded(?MODULE),
   AppFile = filename:dirname(Module) ++ "/" ++ atom_to_list(?MODULE) ++ ".app",
   {ok, [{application, _, List}]} = file:consult(AppFile), 
   % load system configuration
   CfgFile = filename:dirname(Module) ++ "/sys.config",
   case file:consult(CfgFile) of
      {error, _}   -> ok;
      {ok, [Config]} -> 
         lists:foreach(
            fun({App, Cfg}) -> 
               lists:foreach(
                  fun
                     ({K, V}) -> application:set_env(App, K, V);
                     (K)      -> application:set_env(kvs, K, true)
                  end,
                  Cfg
               )
            end, 
            Config
         )
   end,
   % load applications
   Apps = proplists:get_value(applications, List, []),
   lists:foreach(
      fun(X) -> application:start(X) end,
      Apps
   ),
   application:start(?MODULE).
   
%%
%% spawn(Spec) -> ok
%%
%% spawn worker(s) processes assotaited with activity
spawn(Pid, Spec) ->
   lists:foreach(
      fun(Node) ->
         lists:foreach(
            fun(Template) ->
               ok = kvs:put(
                  "kvs:/elata/view", 
                  vid(Node, Pid, Template), 
                  binary_to_list(proplists:get_value(script, Spec))
               )
            end,
            [<<"uri.image">>, <<"latency.image">>, <<"icon.image">>, <<"tcp.image">>, <<"http.image">>, <<"availability.image">>]
         )
      end,
      ek:nodes()
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
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
-module(fe_app).
-behaviour(application).
-author(dmitry.kolesnikov@nokia.com).

-export([
   start/2,
   stop/1
]).

%%%------------------------------------------------------------------   
%%% Default Config
%%%------------------------------------------------------------------   
-define(APPNAME, fe).

%%
%% start application
start(_Type, _Args) ->
   Config = config(?APPNAME, [
      {port, 8080}
   ]),
   case fe_sup:start_link(Config) of
      {ok, Pid} ->
         % discover root(s) 
         {file, Module} = code:is_loaded(?MODULE),
         Root = filename:dirname(filename:dirname(Module)) ++ "/priv/www",
         {ok, DataDir} = application:get_env(be, datapath),
         % web app content
         {ok, _} = kvs:new("kvs:/elata/web", [
            {storage, kvs_fs},
            {root,    Root},
            {type,    file}
         ]),
         % view content
         {ok, _} = kvs:new("kvs:/elata/img", [
            {storage, kvs_fs},
            {root, DataDir ++ "/view"},
            {type, file}
         ]),
         {ok, _} = kvs:new("kvs:/elata/user",    [
            {storage, kvs_dets},
            {file, DataDir ++ "/user-v2.dets"}
         ]),
         {ok, Pid};
      Err ->
         Err
   end.
   
stop(_State) ->
   ok.
   
%%%------------------------------------------------------------------   
%%%
%%% Private Functions
%%%
%%%------------------------------------------------------------------   
config(App, List) ->
   config(App, List, []).
config(App, [{Key, Default} | T], Acc) ->        
   Val = case application:get_env(App, Key) of 
      undefined   -> Default;
      {ok, Value} -> Value
   end,
   config(App, T, [{Key, Val} | Acc]);
config(App, [Key | T], Acc) ->
   case application:get_env(App, Key) of 
      undefined -> config(App, T, Acc);
      {ok, Val} -> config(App, T, [{Key, Val} | Acc])
   end;   
config(_, [], Acc) ->
   Acc.





%% read application config key
get_conf(Key, Default) ->
   case application:get_env(elata_fe, Key) of 
      undefined   -> Default;
      {ok, Value} -> Value
   end.
   
%% return an application configuration
config() ->
   {ok, [
      {port,      get_conf(port, 8080)},
      {datapath,  get_conf(datapath, "/private/tmp/elata")}
   ]}.
   
%% boot application storage                               
boot_storage(Config) ->
   UserDB = get_conf(datapath, "/private/tmp/elata") ++ "/user.dets",
   filelib:ensure_dir(UserDB),
   keyval_store:define(kv_user,    [{module, dets}, {file, UserDB}, notify]).
   

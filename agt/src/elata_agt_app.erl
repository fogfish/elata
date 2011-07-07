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
-module(elata_agt_app).
-behaviour(application).
-author(dmitry.kolesnikov@nokia.com).

-export([
   start/2,
   stop/1
]).

%%%------------------------------------------------------------------   
%%% Default Config
%%%------------------------------------------------------------------   
-define(APPNAME,  elata_agt).
-define(PORT,     8080).  
-define(WORKER,   [
   {idletime,  300000},
   %{idletime,   30000},
   {thinktime,   1000},
   {cycle,         10}
]).


%%
%% start application
start(_Type, _Args) ->
   {ok, Config} = config(),
   boot_storage(Config),
   elata_agt_sup:start_link(Config).
   
stop(_State) ->
   ok.
   
%%%------------------------------------------------------------------   
%%%
%%% Private Functions
%%%
%%%------------------------------------------------------------------   

%% read application config key
get_conf(Key, Default) ->
   case application:get_env(?APPNAME, Key) of 
      undefined   -> Default;
      {ok, Value} -> Value
   end.
   
%% return an application configuration
config() ->
   {ok, [
      {port,   get_conf(port,   ?PORT)},
      {worker, get_conf(worker, ?WORKER)}
   ]}.
   
%% boot application storage   
boot_storage(_Config) ->
   % maps uri to Pids of workers (internal use only)
   keyval_store:define(kv_registry,  [{module, ets}]),
   % temporary persists telemetry results
   keyval_store:define(kv_telemetry, [{module, ets}, notify]),
   agt_kvs_io:subscribe(),
   ok.
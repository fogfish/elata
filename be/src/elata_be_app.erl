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
-module(elata_be_app).
-behaviour(application).
-author(dmitry.kolesnikov@nokia.com).
-include("include/def.hrl").

-export([
   start/2,
   stop/1
]).

%%%------------------------------------------------------------------   
%%% Default Config
%%%------------------------------------------------------------------   
-define(APPNAME, elata_be).
-define(CONF_AGENT,   "/priv/agent.config").  
-define(CONF_JOB,     "/priv/job.config").

-define(TSA_CODEPATH, "/usr/local/macports").
-define(TSA_DATAPATH, "/private/tmp/elata").
-define(TSA_DATABASE, {"127.0.0.1", 42217}).

-define(VIEW,         "/priv/view.config").
-define(VIEW_SCALE,         [
   {{hour,  1},   180000}, %% update every  3 min
   {{hour, 12},   420000}, %% update every  7 min
   {{day,   1},   900000}, %% update every 15 min
   {{day,   3},  1500000}, %% update every 25 min
   {{week,  1},  3300000}, %% update every 55 min
   {{month, 1}, 11100000}, %% update every  3 hour  5 min
   {{month, 6}, 25800000}, %% update every  7 hour 10 min
   {{year,  1}, 41700000}  %% update every 11 hour 35 min
]).


%%
%% start application
start(_Type, _Args) ->
   {ok, Config} = config(),
   boot_storage(Config),
   case elata_be_sup:start_link(Config) of
      {ok, Pid} ->
         start_agent_iface(),
         start_static_view(),
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

%% read application config key
get_conf(Key, Default) ->
   case application:get_env(?APPNAME, Key) of 
      undefined   -> Default;
      {ok, Value} -> Value
   end.

%% return root_dir
root_dir() ->
   {file, Module} = code:is_loaded(?MODULE),
   filename:dirname(filename:dirname(Module)).


%% return an application configuration
config() ->
   {ok, [
      {codepath, get_conf(codepath, ?TSA_CODEPATH)},
      {datapath, get_conf(datapath, ?TSA_DATAPATH)},
      {database, get_conf(database, ?TSA_DATABASE)}
   ]}.
   
%% boot application storage   
boot_storage(_Config) ->
   % configure agents
   keyval_store:define(kv_agent, [{module, ets}]),
   keyval_store:import(kv_agent, get_conf(agent, root_dir() ++ ?CONF_AGENT)),
   % configure job storage
   JobDB = get_conf(datapath, ?TSA_DATAPATH) ++ "/job.dets",
   filelib:ensure_dir(JobDB),
   keyval_store:define(kv_job,  [{module, dets}, {file, JobDB}, notify]),
   keyval_store:import(kv_job,  get_conf(job, root_dir() ++ ?CONF_JOB)),
   % telemetry information (transient data)
   keyval_store:define(kv_telemetry, [{module, ets}]).

%%   
start_agent_iface() ->
   {ok, List} = keyval_store:to_list(kv_agent),
   lists:foreach(
      fun(Agent) ->
         % agent telemetry container
         {ok, _Pid} = be_agt_if_sup:create(Agent)
      end,
      List
   ).
   
%% start uri view rendere
start_static_view() ->
   {ok, Views} = file:consult(get_conf(view, root_dir() ++ ?VIEW)),
   lists:foreach(
      fun(V) -> 
         lists:foreach(
            fun(S) -> be_static_view_sup:create(V, S) end, 
            ?VIEW_SCALE
         )
      end,
      Views
   ).   
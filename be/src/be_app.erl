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
-module(be_app).
-behaviour(application).
-author(dmitry.kolesnikov@nokia.com).

-export([
   start/2,
   stop/1
]).

%%%------------------------------------------------------------------   
%%% Default Config
%%%------------------------------------------------------------------   
-define(APPNAME, be).
-define(CONF_AGENT,   "/priv/agent.config").  
-define(CONF_JOB,     "/priv/job.config").
%
-define(TSA_CODEPATH, "/usr/local/macports").
-define(TSA_DATAPATH, "/private/tmp/elata").
-define(TSA_DATABASE, {"127.0.0.1", 42217}).
%
-define(VIEW,         "/priv/view.config").
-define(VIEW_SCALE,         [
   {"1hours",    180000}, %% update every  3 min
   {"12hours",   420000}, %% update every  7 min
   {"1day",      900000}, %% update every 15 min
   {"3days",    1500000}, %% update every 25 min
   {"1week",    3300000}, %% update every 55 min
   {"1month",  11100000}, %% update every  3 hour  5 min
   {"6months", 25800000}, %% update every  7 hour 10 min
   {"1year",   41700000}  %% update every 11 hour 35 min
]).


%%
%% start application
start(_Type, _Args) ->
   Config = config(?APPNAME, [
      {codepath, "/usr"},
      {datapath, "/var/lib/elata"},
      {sync,     120}
   ]),
   %boot_storage(Config),
   case be_sup:start_link(Config) of
      {ok, Pid} ->
         {file, Module} = code:is_loaded(?MODULE),
         Root = filename:dirname(filename:dirname(Module)) ++ "/priv",
         % process category TODO: dets category
         kvs:new("kvs:/elata/proc", [
            {storage, kvs_dets},
            {file, proplists:get_value(datapath, Config) ++ "/proc.dets"}
         ]), 
         kvs:new("kvs:/elata/doc",  [{storage, kvs_ets}]), 
         % telemetry category
         kvs:new("kvs:/elata/ds",   [
            {storage, kvs_rrd},
            {codepath, proplists:get_value(codepath, Config)},
            {datapath, proplists:get_value(datapath, Config)},
            iocache,
            {daemon, {"127.0.0.1", 42217}},
            {flush,  120},
            {iotime, 60}
         ]),
         % image render category
         kvs:new("kvs:/elata/view", [
            {storage,  be_image},
            {template, Root},
            {codepath, proplists:get_value(codepath, Config)},
            {datapath, proplists:get_value(datapath, Config)},
            {scale,    ?VIEW_SCALE}
         ]),
         kvs:map("kvs:/elata/proc", fun(Pid, Spec) -> be:spawn(Pid, Spec) end),
         
         % bucket to keep a persisten definition of job(s)
         % defined by users
         % kvs_bucket:define(bckt_proc, [{storage, kvs_sys}, event, evtlog]),
         % bucket to keep raw telemetry (sync from agents)
         % kvs_bucket:define(bckt_ds,   [{storage, kvs_sys}]),
         % bucket to keep documents (sync from agents)
         % kvs_bucket:define(bckt_doc,  [{storage, kvs_sys}]),
         %start_agent_iface(),
         %start_static_view(),
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
   
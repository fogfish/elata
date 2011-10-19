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
-module(kvs_app).
-behaviour(application).
-author(dmitry.kolesnikov@nokia.com).

-export([
   start/2,
   stop/1
]).

-define(APPNAME,  kvs).

start(_Type, _Args) -> 
   Config = config(?APPNAME, [
      evt_log, 
      {evt_log_ttl, 7 * 24 * 3600}, 
      {evt_log_chunk, 128}
   ]),
   case kvs_sup:start_link(Config) of
      {ok, Pid} ->
         % start system buckets
         {ok, _} = kvs_sys:start_link([kvs_sys_ref]),
         {ok, _} = kvs_sys:start_link([kvs_sys_bucket]),
         % start evt_log with default config
         case proplists:is_defined(evt_log, Config) of
            true  ->
               kvs_evt_sup:subscribe({kvs_evt_log, [[
                  {ttl,  proplists:get_value(evt_log_ttl,   Config)}, 
                  {chunk,proplists:get_value(evt_log_chunk, Config)}
               ]]}),
               kvs_bucket:define(kvs_evt_log, [{storage, kvs_cache_sup}]);
            false ->
               ok
         end,
         {ok, Pid};
      Other     -> {error, Other}
   end. 

stop(_State) ->
        ok.

%%%------------------------------------------------------------------
%%%
%%%  Private 
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
   
   

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
-module(kvs_evt_log).
-behaviour(gen_event).
-author(dmitry.kolesnikov@nokia.com).
-author(erkki.riekkola@nokia.com).

%%
%% Logs bucket I/O events into event log
%%

-export([
   % api
   config/0,
   % gen_event
   init/1,
   handle_event/2,
   handle_call/2,
   handle_info/2,
   terminate/2,                      
   code_change/3
]).

-record(srv, {
   chunk_size,   % size of chunk
   chunk_ttl,    % chunk ttl
   clock         % clock
}).

config() ->
   gen_event:call(kvs_evt, kvs_evt_log, config).  


%%%------------------------------------------------------------------
%%%
%%%  gen_event
%%%
%%%------------------------------------------------------------------
init([]) ->
   {ok, 
      #srv{
         chunk_size = 512,
         chunk_ttl  = 7 * 24 * 3600,
         clock      = 0
      }
   };
init([Config]) ->
   {ok, 
      #srv{
         chunk_size = proplists:get_value(chunk, Config, 512),
         chunk_ttl  = proplists:get_value(ttl,   Config, 7 * 24 * 3600),
         clock      = 0
      }
   }.
   
handle_event({put, Bucket, Key, Item}, S) -> 
   case proplists:is_defined(evtlog, Bucket) of
      true  -> 
         ChnkId = S#srv.clock div S#srv.chunk_size,
         {chunk, TTL, Log}  = case kvs:get(kvs_evt_log, ChnkId) of
            {error, not_found} -> {chunk, S#srv.chunk_ttl, []};
            {ok, Chnk}         -> Chnk 
         end,
         Name = proplists:get_value(name, Bucket),
         kvs:put(kvs_evt_log, ChnkId, {chunk, TTL, Log ++ [{S#srv.clock, put, Name, Key}]}),
         {ok, S#srv{clock = S#srv.clock + 1}};
      false -> 
         {ok, S}
   end;
handle_event({remove, Bucket, Key, Item}, S) ->
   case proplists:is_defined(evtlog, Bucket) of
      true  ->
         ChnkId = S#srv.clock div S#srv.chunk_size,
         {chunk, TTL, Log}  = case kvs:get(kvs_evt_log, ChnkId) of
            {error, not_found} -> {chunk, S#srv.chunk_ttl, []};
            {ok, Chnk}         -> Chnk 
         end,
         Name = proplists:get_value(name, Bucket),
         kvs:put(kvs_evt_log, ChnkId, {chunk, TTL, Log ++ [{S#srv.clock, remove, Name, Key}]}),
         {ok, S#srv{clock = S#srv.clock + 1}};
      false -> 
         {ok, S}
   end;
handle_event(Evt, State) ->
   {ok, State}.
   
handle_call(config, State) ->
   {ok, {State#srv.chunk_size, State#srv.chunk_ttl, State#srv.clock}, State};
handle_call(_Req, State) ->
   {ok, undefined, State}.
   
handle_info(_Msg, State) ->
   {ok, State}.
   
terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) -> 
   {ok, State}.   
   
   
%%%------------------------------------------------------------------
%%%
%%%  Private 
%%%
%%%------------------------------------------------------------------   
 

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
-module(be_agt_if).
-behaviour(gen_server).
-author(dmitry.kolesnikov@nokia.com).
-include("include/def.hrl").

%%
%% ELATA: Agent Interface (worker) 
%%

-export([
   % api
   start_link/1,
   %% gen_server
   init/1, 
   handle_call/3,
   handle_cast/2, 
   handle_info/2, 
   terminate/2, 
   code_change/3 
]).

-define(SYNC,     300000).
-define(RECONF,  1800000).

-record(srv, {
   agent   
}).

%%
%% start
start_link(Agent) ->
   gen_server:start_link(?MODULE, [Agent], []).

init([Agent]) ->
   timer:send_after(kvset:get(sync, Agent, ?SYNC),   sync),
   timer:send_after(1000, reconf), %% reconf asap (health status is unknown)
   {ok,
      #srv{
         agent   = Agent
      }
   }.

handle_call(_Req, _From, State) ->
   {reply, undefined, State}.
handle_cast(_Req, State) ->
   {noreply, State}.
handle_info(sync,   #srv{agent = A} = State) ->   
   sync_agt_telemetry(A),
   timer:send_after(kvset:get(sync, A, ?SYNC), sync),
   {noreply, State};
handle_info(reconf, #srv{agent = A} = State) ->   
   config_agt(A),
   timer:send_after(kvset:get(reconf, A, ?RECONF), reconf),
   {noreply, State};
handle_info(_Msg, State) ->
   {noreply, State}.
   
terminate(_Reason, _State) ->
   ok.
   
code_change(_OldVsn, State, _Extra) ->
   {ok, State}.   
 
%%%------------------------------------------------------------------
%%%
%%% Server Implementation
%%%
%%%------------------------------------------------------------------
sync_agt_telemetry(Agent) ->
   % Bucket = binary_to_atom(Id, utf8),
   Id   = kvset:name(Agent),
   Tele = get_from_agt(Agent),
   %% telemetry is a list of kvset
   lists:foreach(
      fun(Set) ->
         % update feed
         Uri = kvset:name(Set),
         {ok, Data0} = case keyval_store:lookup(kv_telemetry, Uri) of
            {error, _} -> {ok, kvset:new(Uri)};
            Result     -> Result
         end,
         % use location Id as merge point
         Data1 = kvset:append([Id, telemetry], Set, Data0),
         keyval_store:insert(kv_telemetry, Data1),
         write_to_tsa(Id, Set)
      end,
      Tele
   ).
   
config_agt(Agent) ->
   {ok, Jobs} = keyval_store:to_list(kv_job),
   % maps job into uri list
   Uris = lists:map(
      fun (Set) -> kvset:name(Set) end,
      Jobs
   ),
   put_to_agt(Agent, Uris).

%%%------------------------------------------------------------------
%%%
%%% Private function
%%%
%%%------------------------------------------------------------------

%% retrive telemetry from agent
get_from_agt(Agent) ->
   {Host, Port} = kvset:get(host, Agent),
   set_proxy(Agent),
   Uri     = "http://" ++ Host ++ ":" ++ integer_to_list(Port) ++ "/telemetry",
   {ok,{ {_, 200, _}, _, Msg}} = httpc:request(
      get, 
      {
         Uri, 
         [{"accept", "application/octet-stream"}, {"connection", "close"}]
      },
      [],
      []
   ),
   binary_to_term(list_to_binary(Msg)).
   
%% post configuration to agent
put_to_agt(Agent, List) ->
   {Host, Port} = kvset:get(host, Agent),
   set_proxy(Agent),
   Uri     = "http://" ++ Host ++ ":" ++ integer_to_list(Port) ++ "/config",
   {ok,{ {_, 204, _}, _, _Msg}} = httpc:request(
      put,
      {
         Uri,
         [{"connection", "close"}], 
         "application/octet-stream",
         binary_to_list(term_to_binary(List))
      },
      [],
      []
   ).
      
set_proxy(Agent) ->
   case kvset:get(proxy, Agent) of
      {Phost, Pport} ->
         httpc:set_options([
            {proxy, {{Phost, Pport}, ["localhost"]}}
         ]);
      _ ->
         ok
   end.
   
%% persist results into TSA storage
write_to_tsa(Bucket, Set) ->
   Uri  = kvset:name(Set),
   Hash = erlang:phash2(Uri),
   {MSec, Sec, _} = erlang:now(),
   
   %% iterate telemetry results
   %% each uri has latency class, each class has multiple data streams 
   lists:foreach(
      fun(Class) ->
         lists:foreach(
            fun({Stream, Value}) ->
               Sname = lists:flatten(
                  io_lib:format(
                     '~.16x/~s/~s/~s.rrd', 
                     [Hash, "0x", Bucket, Class, Stream]
                  )
               ),
               %% value has to be accomplished with time stamp
               %% TODO: retrive agent local timestamp
               tsa_db:write(Sname, {MSec * 1000000 + Sec, Value})
            end,
            kvset:get(Class, Set)
         )
      end,
      kvset:keys(Set)
   ).
   
%%%------------------------------------------------------------------
%%%
%%% Private functions
%%%
%%%------------------------------------------------------------------

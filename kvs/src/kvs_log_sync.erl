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
-module(kvs_log_sync).
-behaviour(gen_server).
-author(dmitry.kolesnikov@nokia.com).

%%
%% Log-based bucket synchronization  
%%

%% TODO: full resync

-export([
   start_link/0,
   start_link/1,
   %% gen_server
   init/1, 
   handle_call/3,
   handle_cast/2, 
   handle_info/2, 
   terminate/2, 
   code_change/3
]).

-define(KVS_SYNC_EP, "/kvs/sync/10").
-record(srv, {
   thinktime,  % peer sync timeout
   nodes       % sync nodes and they last clock
}).

%%
%% debug macro
-ifdef(DEBUG).
-define(DEBUG(M), error_logger:info_report(M)).
-else.
-define(DEBUG(M), true).
-endif.

%%
%%
start_link() ->
   start_link(30).
start_link(Think) ->
   gen_server:start_link({local, ?MODULE}, ?MODULE, [Think], []).
  
init([Think]) ->
   ek:monitor(),
   ek:listen(?KVS_SYNC_EP),
   timer:send_after(Think * 1000, sync),
   {ok, 
      #srv{
         thinktime = Think * 1000,
         nodes     = []
      }
   }.

handle_call(_Req, _From, State) ->
   {reply, undefined, State}.   
   
handle_cast(_Req, State) ->
   {noreply, State}.

%%
%% initiate sync
handle_info(sync, State) ->
   lists:map(
      fun(N) ->
         case lists:keyfind(N, 1, State#srv.nodes) of
            {_Node, Clock} -> req_node_sync(N, Clock);
            false          -> req_node_sync(N, -1)      
         end
      end,
      ek:nodes()
   ),
   timer:send_after(State#srv.thinktime, sync),
   {noreply, State};

%%   
%% handle sync request, all events from Clock   
handle_info({sync_req, From, Clock}, State) ->
   {ChnkSize, ChnkTTL, SrvClk} = kvs_evt_log:config(),
   ChnkId = Clock  div ChnkSize,  % Id of log chunk to start with
   LastId = SrvClk div ChnkSize,  % Id of log chunk to finish
   ?DEBUG([
      {sync, ind},
      {node, From},
      {chunk, {ChnkId, LastId}}
   ]),
   rsp_node_sync(From, ChnkId, LastId), 
   {noreply, State};

%%
%% handle sync response
handle_info({sync_rsp, From, Log}, State) ->
   % what is last log position
   Clock = case lists:keyfind(From, 1, State#srv.nodes) of
     {Node, Clk} -> Clk;
     false       -> -1
   end,
   % replay log 
   NClock = replay_log(Clock, Log),
   ?DEBUG([
      {sync, rsp},
      {node, From},
      {clock, {Clock, NClock}}
   ]),
   % update log position
   Nodes  = [{From, NClock} | lists:keydelete(From, 1, State#srv.nodes)],
   {noreply, State#srv{nodes = Nodes}};
   
%%
%% handle node join/leave
handle_info({join,  Node}, State) ->
   {noreply, State};
handle_info({leave, Node}, State) ->
   {noreply, State};
handle_info(ek_evt_failed, State) ->
   ek:monitor(),
   {noreply, State};
handle_info(_Msg, State) ->
   {noreply, State}.   
   
terminate(_Reason, State) ->
   ek:listen({drop, ?KVS_SYNC_EP}),
   ok.
   
code_change(_OldVsn, State, _Extra) ->
   {ok, State}.     
   
   
%%%------------------------------------------------------------------
%%%
%%%  Private 
%%%
%%%------------------------------------------------------------------   

%%
%% Initializes a sync procedure with node
req_node_sync(Node, Clock) ->
   ?DEBUG([
      {sync,  req},
      {clock, Clock},
      {node,  Node}
   ]),
   ek:send(Node ++ ?KVS_SYNC_EP, {sync_req, ek:node(), Clock}).   

%%
%% Response on sync request
rsp_node_sync(Node, ChnkId, LastId) when ChnkId > LastId ->
   % sync completed
   ?DEBUG([
      {sync, completed},
      {node, Node}
   ]),
   ok;
rsp_node_sync(Node, ChnkId, LastId) ->
   % read chunk
   case kvs:get(kvs_evt_log, ChnkId) of
      {error, not_found} ->
         % evt_log position is gone a full resync is required
         ?DEBUG([
            {sync, {req, full}},
            {node, Node}
         ]),
         ok;
      {ok, {chunk, _, Log}}        ->
         ?DEBUG([
            {sync, {chunk, ChnkId, LastId}},
            {node, Node}
         ]),
         ek:send(Node ++ ?KVS_SYNC_EP, {sync_rsp, ek:node(), Log}),
         rsp_node_sync(Node, ChnkId + 1, LastId)
   end.  

%%
%%
replay_log(Clock, Log) ->
   lists:foldl(
      fun
      ({put, EvtClk, Bckt, Key, Item}, Clk) ->
         if
            EvtClk =:= Clk + 1 -> 
               kvs:put(Bckt, Key, Item),
               EvtClk;
            true ->
               Clk
         end;
      ({remove, EvtClk, Bckt, Key}, Clk) ->
         if
            EvtClk =:= Clk + 1 ->
               kvs:remove(Bckt, Key),
               EvtClk;
            true ->
               Clk
         end
      end,
      Clock,
      Log
   ).
   

 

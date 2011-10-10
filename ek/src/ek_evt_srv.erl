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
-module(ek_evt_srv).
-behaviour(gen_server).
-author(dmitry.kolesnikov@nokia.com).

%%
%% Erlang Cluster Events (based on pure messaging)
%%

-export([
   % api
   start_link/0,
   subscribe/1,
   unsubscribe/1,
   join/1,
   leave/1,
   %% gen_server
   init/1, 
   handle_call/3,
   handle_cast/2, 
   handle_info/2, 
   terminate/2, 
   code_change/3
]).

   
%%%------------------------------------------------------------------
%%%
%%%  API 
%%%
%%%------------------------------------------------------------------   
subscribe(Pid) ->
   gen_server:call(?MODULE, {subscribe, Pid}).
   
unsubscribe(Pid) ->
   gen_server:call(?MODULE, {unsubscribe, Pid}).
   
join(Node) ->
   gen_server:cast(?MODULE, {join, Node}).
   
leave(Node) ->
   gen_server:cast(?MODULE, {leave, Node}).


%%%------------------------------------------------------------------
%%%
%%%  gen_server 
%%%
%%%------------------------------------------------------------------   
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
  
init([]) ->
   {ok, []}.

handle_call({subscribe, Pid}, _From, State) ->   
   {reply, ok, [Pid | lists:delete(Pid, State)]};
handle_call({unsubscribe, Pid}, _From, State) ->
   {reply, ok, lists:delete(Pid, State)};
handle_call(_Req, _From, State) ->
   {reply, undefined, State}.   
   
handle_cast({join, Node}, State) ->
   [P ! {join, Node} || P <- State],
   {noreply, State};
handle_cast({leave, Node}, State) ->
   [P ! {leave, Node} || P <- State],
   {noreply, State};
handle_cast(_Req, State) ->
   {noreply, State}.

handle_info(_Msg, State) ->
   {noreply, State}.   
   
terminate(_Reason, State) ->
   [P ! ek_evt_failed || P <- State],
   ok.
   
code_change(_OldVsn, State, _Extra) ->
   {ok, State}.     
   
   
%%%------------------------------------------------------------------
%%%
%%%  Private 
%%%
%%%------------------------------------------------------------------   
                                                               

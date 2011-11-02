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
-module(kvs_sync_ht_tx).
-behaviour(gen_fsm).
-author(dmitry.kolesnikov@nokia.com).

%%%
%%% Bucket Sync TX based on hash tree
%%%

-export([
   start_link/1,
   % gen_fsm
   init/1,
   'DIFF'/2,
   'DATA'/2,
   handle_event/3,
   handle_sync_event/4,
   handle_info/3,
   terminate/3,
   code_change/4
]).



%%%------------------------------------------------------------------   
%%%
%%% gen_fsm
%%%
%%%------------------------------------------------------------------
start_link(Bucket) ->
   gen_fsm:start_link(?MODULE, [Bucket]).
   
init([Bucket]) ->
   {ok, 'DIFF', []}.

%%%
%%%
'DIFF'(_Evt, State) ->
   {next_state, 'DIFF', State}.
   
%%%
%%%
'DATA'(_Evt, State) ->
   {next_state, 'DATA', State}.

%%%
%%% 
handle_event(Msg, Name, State) ->
   erlang:apply(?MODULE, Name, [Msg, State]).
   
handle_info(Msg, Name, State) ->
   erlang:apply(?MODULE, Name, [Msg, State]).

terminate(Reason, Name, State) ->
   ok.
   
code_change(_OldVsn, Name, State, _Extra) ->
   {ok, Name, State}.    
   
handle_sync_event(_Req, _From, Name, State) ->
   {reply, {error, not_supported}, Name, State}.   
   
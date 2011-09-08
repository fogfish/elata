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
-module(kvs_evt_handler_sup).
-behaviour(gen_server).

%%
%% Any gen_event handler failure causes silent removal of the handler 
%% from event manager pool. Thus supervision of event handlers is mandatory
%% to achive fault tolerance
%%
-export([
   start_link/1, 
   %% gen_server
   init/1, 
   handle_call/3,
   handle_cast/2, 
   handle_info/2, 
   terminate/2, 
   code_change/3
]).
   
start_link(Handler) ->
   gen_server:start_link({local, ?MODULE}, ?MODULE, [Handler], []).

init([Handler]) ->
   kvs_evt:subscribe(Handler),
   {ok, Handler}.
   
handle_call(_Req, _From, State) ->
   {reply, undefined, State}.
handle_cast(_Req, State) ->
   {noreply, State}.

handle_info({gen_event_EXIT, _Handler, normal}, State) ->
   {stop, normal, State};
handle_info({gen_event_EXIT, _Handler, shutdown}, State) ->
   {stop, normal, State};
handle_info({gen_event_EXIT, _Handler, _Reason}, State) ->
   {stop, {error, event_handler}, State};
handle_info(_Msg, State) ->
   {noreply, State}.
   
terminate(_Reason, _State) ->
   ok.
   
code_change(_OldVsn, State, _Extra) ->
   {ok, State}.        
   
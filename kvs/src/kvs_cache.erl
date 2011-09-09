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
-module(kvs_cache).
-behaviour(gen_server).
-author(dmitry.kolesnikov@nokia.com).

%%
%%  
%%

-export([
   start_link/2,
   %% gen_server
   init/1, 
   handle_call/3,
   handle_cast/2, 
   handle_info/2, 
   terminate/2, 
   code_change/3 
]).

%%
%%
start_link(Key, Item) ->
  gen_server:start_link(?MODULE, [Key, Item], []).
  
init([Key, Item]) ->
   kvs_reg:register(Key, self()),
   {ok, {Key, Item}}.

   
handle_call({kvs_set, Item}, _From, {Key, _}) ->
   {reply, ok, {Key, Item}};
handle_call(kvs_get, _From, {Key, Item}) ->
   {reply, {ok, Item}, {Key, Item}};
handle_call(_Req, _From, State) ->
   {reply, undefined, State}.
handle_cast(kvs_destroy, State) ->
   {stop, normal, State};
handle_cast(_Req, State) ->
   {noreply, State}.

handle_info(_Msg, State) ->
   {noreply, State}.
   
terminate(_Reason, {Key, _}) ->
   kvs_reg:unregister(Key),
   ok.
   
code_change(_OldVsn, State, _Extra) ->
   {ok, State}.     

%%%------------------------------------------------------------------   
%%%
%%% Private functions
%%%
%%%------------------------------------------------------------------


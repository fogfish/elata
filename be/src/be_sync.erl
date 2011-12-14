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
-module(be_sync).
-behaviour(gen_server).
-author(dmitry.kolesnikov@nokia.com).

%%
%% ELATA: Back-end to Agent synchronization
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

-record(srv, {
   thinktime   
}).

%%
%% start
start_link(Config) ->
   gen_server:start_link({local, ?MODULE}, ?MODULE, [Config], []).

init([Config]) ->
   % ignore crash of underlying transactions this is normal
   erlang:process_flag(trap_exit, true),
   {ok,
      #srv{
         thinktime   = proplists:get_value(sync, Config) * 1000
      },
      1000
   }.

handle_call(_Req, _From, S) ->
   {reply, undefined, S}.
   
handle_cast(_Req, S) ->
   {noreply, S}.
   
handle_info(timeout, #srv{thinktime = T} = S) ->
   lists:foreach(
      fun(Uri) ->
         catch(
            kvs_sync_ht_tx:start_link(
               master, 
               ek_uri:set(
                  authority,
                  ek_uri:get(authority, Uri), 
                  "kvs:/elata/proc"
               )
            )
        )
      end,
      ek:nodes()
   ),
   timer:send_after(T,   timeout),
   {noreply, S};
handle_info(_Msg, State) ->
   {noreply, State}.
   
terminate(_Reason, _State) ->
   ok.
   
code_change(_OldVsn, State, _Extra) ->
   {ok, State}.   
   
%%%------------------------------------------------------------------
%%%
%%% Private functions
%%%
%%%------------------------------------------------------------------

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
-module(repl_replicator).
-behaviour(gen_server).

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

-record(srv, {
	ts,
	receiver
}). 

%%
%% start
start_link(Receiver) ->
   error_logger:info_report(["Replicate:start:", [ Receiver] ]),
   gen_server:start_link(?MODULE, [Receiver], []).

init([Receiver]) ->
   error_logger:info_report(["Replicate:init:", [ Receiver] ]),
   timer:send_after(100,   sync),	%% start syncing fast
   {Megasecs, Secs, Microsecs} = erlang:now(),
   TS = Megasecs * 1000000000 + Secs *1000 + Microsecs,
   {ok, 
	#srv{
	    ts = TS,
	    receiver = Receiver
	}
   }.

handle_call(_Req, _From, State) ->
   {reply, undefined, State}.
handle_cast(_Req, State) ->
   {noreply, State}.
handle_info(sync, State) ->   
   %% sync the items in ets to client...
   List = ets:match(repl_tslog, {'$1', '$2', '$3', '$4', '$5'}),
   error_logger:info_report(["Replicate: List:", [ List] ]),
   timer:send_after(?SYNC, sync),
   {noreply, State};

handle_info(reconf, State) ->   
   {noreply, State};
handle_info(_Msg, State) ->
   {noreply, State}.
   
terminate(_Reason, _State) ->
   ok.
   
code_change(_OldVsn, State, _Extra) ->
   {ok, State}.   


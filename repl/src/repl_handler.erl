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
-module(repl_handler).
-behaviour(gen_event).

-export([
   init/1,
   handle_event/2,
   handle_call/2,
   handle_info/2,
   terminate/2,
   code_change/3
]).
%% tried to use record, but lots of difficulties...
-record(b_log, {
   ts,       %% timestamp as key
   bucket,   %%
   key,      %%
   op,       %% operation: put or remove
   item      %% the actual item
}). 


%%%------------------------------------------------------------------
%%%
%%%  gen_event
%%%
%%%------------------------------------------------------------------
init([]) -> 
   ets:new(repl_tslog, [public, named_table, ordered_set ]),
   {ok, []}.
   
handle_event({put, Bucket, Key, Item}, State) ->
   %% TODO: 
   %% have configuration to check whether items of this bucket are to be monitored or not
   {Megasecs, Secs, Microsecs} = erlang:now(),
   TS = Megasecs * 1000000000 + Secs *1000 + Microsecs,
%%   ets:insert(repl_tslog, {#b_log{ts=TS, op=put, bucket=Bucket, key=Key, item=Item}}),
   ets:insert(repl_tslog, {TS, Bucket, Key, put, Item}),
   error_logger:info_report(["Handle Event: put: ets list:", [ ets:tab2list(repl_tslog)] ]),

   {ok, State};

handle_event({remove, Bucket, Key, Item}, State) ->
   {Megasecs, Secs, Microsecs} = erlang:now(),
   TS = Megasecs * 1000000000 + Secs *1000 + Microsecs,
%%   ets:insert(repl_tslog, {#b_log{ts=TS, op=remove, bucket=Bucket, key=Key, item=Item}}),
   ets:insert(repl_tslog, {TS, Bucket, Key, remove, Item}),
   error_logger:info_report(["Handle Event: remove: ets list:", [ ets:tab2list(repl_tslog)] ]),

   {ok, State};

handle_event(Evt, State) ->
   {ok, State}.
   
handle_call(_Req, State) ->
   {ok, undefined, State}.
   
handle_info(_Msg, State) ->
   {ok, State}.
   
terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) -> 
   {ok, State}.


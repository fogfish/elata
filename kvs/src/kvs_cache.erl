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
   start_link/3,
   %% gen_server
   init/1, 
   handle_call/3,
   handle_cast/2, 
   handle_info/2, 
   terminate/2, 
   code_change/3 
]).

-record(srv, {
   ttl,       % time-to-live in seconds
   timestamp, % timestamp of last item i/o activity
   cat,       % name of category where key belongs
   key,       % key
   val        % value
}).

%%
%% debug macro
-ifdef(DEBUG).
-define(DEBUG(M), error_logger:info_report([{?MODULE, self()}] ++ M)).
-else.
-define(DEBUG(M), true).
-endif.

%%
%%
start_link(Spec, Key, Val) ->
  gen_server:start_link(?MODULE, [Spec, Key, Val], []).
  
init([Spec, Key, Val]) ->
   Cat = proplists:get_value(uri, Spec),
   gen_kvs:val_init(Cat, Key),
   {ok, 
      #srv{
         ttl=ttl_timer(gen_kvs:get_val_prop(Cat, ttl, Val)),
         timestamp=timestamp(),
         cat=Cat, 
         key=Key,
         val=Val
      }
   }. 
   
handle_call({kvs_put, Key, Val}, _From, S) ->
   {reply, ok, 
      S#srv{
         ttl=ttl_timer(gen_kvs:get_val_proc(S#srv.cat, ttl, Val)), 
         timestamp=timestamp(), 
         val=Val
      }
   };
handle_call({kvs_get, Key}, _From, S) ->
   {reply, {ok, S#srv.val}, S#srv{timestamp=timestamp()}};
handle_call(_Req, _From, State) ->
   {reply, undefined, State}.
   
handle_cast({kvs_remove, Key}, State) ->
   {stop, normal, State};
handle_cast(_Req, State) ->
   {noreply, State}.
 
handle_info(expired, S) ->
   case S#srv.ttl of
      undefined -> 
         {noreply, S};
      TTL       ->
         Now      = timestamp(),
         Deadline = S#srv.timestamp + S#srv.ttl,
         ?DEBUG([
            {ttl, S#srv.ttl}, 
            {now, Now}, 
            {deadline, Deadline},
            {key, S#srv.key}
         ]),
         if 
            Deadline =< Now -> 
               {stop, normal, S};
            Deadline >  Now -> 
               ttl_timer(TTL),
               {noreply, S} 
         end
   end;
handle_info(_Msg, State) ->
   {noreply, State}.
   
terminate(_Reason, S) ->
   gen_kvs:val_terminate(S#srv.cat, S#srv.key),
   ok.
   
code_change(_OldVsn, State, _Extra) ->
   {ok, State}.     

%%%------------------------------------------------------------------   
%%%
%%% Private functions
%%%
%%%------------------------------------------------------------------

% return unix timestamp
timestamp() ->
   {Mega, Sec, _Micro} = erlang:now(),
   Mega * 1000000 + Sec.
   
% starts time-to-live timer   
ttl_timer(undefined) ->
   undefined;
ttl_timer(TTL) ->   
   ?DEBUG([{ttl, TTL * 1000}]),
   timer:send_after(TTL * 1000, expired),
   TTL.

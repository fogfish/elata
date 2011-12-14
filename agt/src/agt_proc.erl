%%
%%   Copyright (c) 2011, Nokia Corporation
%%   All rights reserved.
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
-module(agt_proc).
-behaviour(gen_server).
-author(dmitry.kolesnikov@nokia.com).

%%
%% ELATA: Agent Process
%%        it is periodcally executed sample and hold process
%%
%% key - unique proc identity SHA1( script )
%% val - process descriptor proplists
%%    script    - [Uri, Opts]
%%    thinktime - time between probes
%%    authority - owner node
%%    [ttl]     -

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

%% Process internall state
-record(srv, {
   cat,         %% bucket descriptor
   key,         %% unique job identifier
   proc,        %% job description                  
   
   timestamp,   %% unix timestamp when job is started (in seconds)  
   pipeline     %% script executor 
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
start_link(Spec, Key, Proc) ->
   gen_server:start_link(?MODULE, [Spec, Key, Proc], []).
   
%%
%% Init
init([Spec, Key, Proc]) ->
   ?DEBUG([{cat, Spec}, {key, Key}, {proc, Proc}]),
   Cat = proplists:get_value(uri, Spec),
   gen_kvs:val_init(Cat, Key),
   t_thinktime(Proc),
   % return a state
   {ok,
      #srv{
         cat = Spec,
         key = Key,
         proc= Proc,
         timestamp = timestamp(),
         pipeline  = emc:c(runnable, emc_pf:new(emc_id, 120), agt_hof:pipeline(Key))
       }
    }.
         
%%% set
handle_call({kvs_put, _Key, Proc}, _From, State) ->
   {reply, ok, 
      State#srv{
         proc = Proc,
         timestamp = timestamp()
      }
   };
handle_call({kvs_get, _Key}, _From, State) ->
   {reply, {ok, State#srv.proc}, State};
handle_call(_Req, _From, State) ->
   {reply, undefined, State}.
handle_cast({kvs_remove, _Key}, State) ->
   {stop, normal, State};
handle_cast(_Req, State) ->
   {noreply, State}.

handle_info(thinktime, #srv{proc = Proc, pipeline = Fun} = S) ->
   % run a script and collect telemetry
   Fun([
      % uri
      proplists:get_value(script, Proc),
      % opts
      [
         {header, proplists:get_value(http,  Proc)},
         {proxy,  proplists:get_value(proxy, Proc)}
      ]
   ]),
   % re-schedule a process
   case proplists:get_value(ttl, Proc) of
      undefined ->
         t_thinktime(Proc),
         {noreply, S};      
      TTL       ->
         Now  = timestamp(),
         Dead = S#srv.timestamp + TTL,
         if 
            Now < Dead ->
               t_thinktime(Proc),
               {noreply, S};
            true       ->
               {stop, normal, S}
         end
   end;
handle_info(_Msg, State) ->
   {noreply, State}.
   
terminate(_Reason, S) ->
   ?DEBUG([{process, terminated} | S#srv.proc]),
   Cat = proplists:get_value(uri, S#srv.cat),
   gen_kvs:val_terminate(Cat, S#srv.key),
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

% starts think timer   
t_thinktime(Proc) ->   
   Thinktime = proplists:get_value(thinktime, Proc, 300),
   timer:send_after(Thinktime * 1000, thinktime).
   
   
%%   
%% binary to hex
bin_to_hex(Bin) ->
   bin_to_hex(Bin, "").
   
bin_to_hex(<<>>, Acc) ->
   Acc;
bin_to_hex(<<X:8, T/binary>>, Acc) ->  
   bin_to_hex(T, Acc ++ [to_hex(X div 16), to_hex(X rem 16)]).
   
to_hex(X) when X < 10 ->
   $0 + X;
to_hex(X) ->
   $a + (X - 10).

%%
%% hex to binary
hex_to_bin(Hex) ->
   hex_to_bin(Hex, <<>>).

hex_to_bin([], Acc) ->
   Acc;
hex_to_bin([H, L | T], Acc) ->
   V = to_int(H) * 16 + to_int(L),
   hex_to_bin(T, <<Acc/binary, V>>).

to_int(C) when C >= $a, C =< $f ->
   10 + (C - $a);
to_int(C) when C >= $0, C =< $9 ->
   C - $0.   
   

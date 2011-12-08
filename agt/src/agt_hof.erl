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
-module(agt_hof).
-author(dmitry.kolesnikov@nokia.com).

%%
%% High-order functions: Elata agent
%%
-export([
   pipeline/2
]).

%%
%% agent measurment pipeline
pipeline(Owner, Key) ->
   emc:seq([
      hof_perf:net(),                 % latency measurment pipeline
      fun telemetry/2,                % telemetry aggregation
      fun(Tele, Uri, Doc) ->          % telemetry persistency
         store(Owner, Key, Tele, Uri, Doc) 
      end  
   ]).


%%
%% accumulates telemetry
telemetry(PfStat, SockStat) ->
   Uri  = lists:foldl(fun({_, V}, A) -> A + V end, 0, PfStat),
   Stat = [{uri, Uri} | PfStat ++ SockStat], 
   {ok, [Stat]}.
   
%%
%%
store(Owner, Key, Tele, Uri, {Code, HTTP, Doc}) ->
   lists:foreach(
      fun({Tag, Value}) ->
         Sfx = atom_to_binary(Tag, utf8),
         Tid = {http, ek:node(), <<"/", Key/binary, "/", Sfx/binary>>},
         ok  = kvs:put({kvs, ek_uri:authority(Owner), <<"/elata/ds">>}, Tid, {timestamp(), Value})
      end,
      Tele
   ),
   Tid = {http, ek:node(), <<"/", Key/binary, "/code">>},
   ok  = kvs:put({kvs, ek_uri:authority(Owner), <<"/elata/ds">>}, Tid, {timestamp(), Code}),
   ok  = kvs:put({kvs, ek_uri:authority(Owner), <<"/elata/rsp">>}, {http, ek:node(), <<"/", Key/binary>>}, HTTP),
   ok  = kvs:put({kvs, ek_uri:authority(Owner), <<"/elata/doc">>}, {http, ek:node(), <<"/", Key/binary>>}, Doc),
   {ok, []}.



% return unix timestamp
timestamp() ->
   {Mega, Sec, _Micro} = erlang:now(),
   Mega * 1000000 + Sec.   
   
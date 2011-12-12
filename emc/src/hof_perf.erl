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
-module(hof_perf).
-author(sergey.boldyrev@nokia.com).
-author(dmitry.kolesnikov@nokia.com).

%%
%% High order-function: Performance
%%

-export([
   net/0,
   filter/2
]).

%%-------------------------------------------------------------------
%%
%% Request network service
%% 
%% hof(Uri, Req) -> [Stat, {Schema, Status, Protocol, Payload}]
%%   Schema = atom()
%%   Status = 
%%
%%-------------------------------------------------------------------

%%
%% Request Network Service
net() ->
   emc:seq([
      fun ek_uri:new/1,
      emc:alt([
         hof_inet:dns(),
         hof_inet:tcp(),
         hof_inet:ssl(),
         hof_http:http(),
         hof_http:https()
      ]),
      emc:'='(emc_pf),
      fun(X, Stat) -> 
         hof_perf:filter(X ++ Stat, [
            {<<"hof_inet:dns/2">>, dns},
            {<<"hof_inet:tcp/2">>, tcp},
            {<<"hof_inet:ssl/3">>, ssl},
            {<<"hof_http:wait/2">>, ttfb},
            {<<"hof_http:recv/3">>, ttmr},
            {recv_oct, size},
            {recv_avg, chnk},
            {recv_cnt, pckt}
         ]) 
      end
   ]).

                                  
   
%%
%% Filters performance results 
filter(Pf, Sample) ->
   {ok, 
      [lists:foldl(
         fun({Smpl, Tag}, Acc) ->
            case proplists:get_value(Smpl, Pf) of
               undefined -> Acc;
               Val       -> [{Tag, Val} | Acc]
            end
         end,
         [],
         Sample
      )]
   }.
   

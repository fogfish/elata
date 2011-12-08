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
%% High order-function: latency measurment
%%

-export([
   net/0,
   filter/2
]).

%%
%% Measures latency for various network services
net() ->
   emc:seq([
      fun ek_uri:new/1,
      emc:alt([
         %% dns pipeline
         emc:seq([
            fun
               ({dns,_,_}=Uri) -> {ok, [Uri]};
               (_)             -> {error, uri_scheme}
            end,
            fun hof_inet:dns/1,
            fun (IP, Uri) ->
               % TODO: fix pipeline dimensions
               {ok, [Uri, []]}
            end
         ]),
         %% tcp pipeline
         emc:seq([
            fun
               ({tcp,_,_}=Uri) -> {ok, [Uri]};
               (_)             -> {error, uri_scheme}
            end,
            fun hof_inet:dns/1,
            fun hof_inet:tcp/3,
            fun hof_inet:statistic/1,
            fun hof_inet:close/1
         ]),
         %% ssl pipeline
         emc:seq([
            fun
               ({ssl,_,_}=Uri) -> {ok, [Uri]};
               (_)             -> {error, uri_scheme}
            end,
            fun hof_inet:dns/1,
            fun hof_inet:tcp/3,
            fun hof_inet:ssl/3,
            fun hof_inet:statistic/1,
            fun hof_inet:close/1
         ]),
         %% http pipeline
         emc:seq([
            fun
               ({http,_,_}=Uri) -> {ok, [Uri]};
               (_)              -> {error, uri_scheme}
            end,
            fun hof_inet:dns/1,
            fun hof_inet:tcp/3,
            fun hof_http:get/3,
            fun hof_http:recv/2,
            fun hof_http:recv/3,
            fun hof_http:response/3,
            fun hof_inet:statistic/1,
            fun hof_inet:close/1
         ]),
         %% https pipeline
         emc:seq([
            fun
               ({https,_,_}=Uri) -> {ok, [Uri]};
               (_)               -> {error, uri_scheme}
            end,
            fun hof_inet:dns/1,
            fun hof_inet:tcp/3,
            fun hof_inet:ssl/3,
            fun hof_http:get/3,
            fun hof_http:recv/2,
            fun hof_http:recv/3,
            fun hof_http:response/3,
            fun hof_inet:statistic/1,
            fun hof_inet:close/1
         ])
      ]),
      emc:'='(emc_pf),
      fun(X) -> 
         hof_perf:filter(X, [
            {<<"hof_inet:dns/1">>, dns},
            {<<"hof_inet:tcp/3">>, tcp},
            {<<"hof_inet:ssl/3">>, ssl},
            {<<"hof_http:recv/2">>, ttfb},
            {<<"hof_http:recv/3">>, ttmr}
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
   

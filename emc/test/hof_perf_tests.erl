%
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
-module(hof_perf_tests).
-author(dmitry.kolesnikov@nokia.com).
-include_lib("eunit/include/eunit.hrl").

   
dns_test() ->
   C = emc:c(runnable, emc_pf:new(emc_id, 0), hof_perf:net()),
   [Pf, {dns, 0, _, _}] = C(["dns://www.google.com"]),
   ?assert(
      undefined =/= proplists:get_value(dns, Pf)
   ).

tcp_test() ->
   C = emc:c(runnable, emc_pf:new(emc_id, 0), hof_perf:net()),
   [Pf, {tcp, 0, _, _}] = C(["tcp://www.google.com:80"]),
   ?assert(
      undefined =/= proplists:get_value(dns, Pf)
   ),
   ?assert(
      undefined =/= proplists:get_value(tcp, Pf)
   ).    

ssl_test() ->
   ssl:start(),
   C = emc:c(runnable, emc_pf:new(emc_id, 0), hof_perf:net()),
   [Pf, {ssl, 0, _, _}] = C(["ssl://www.google.com:443"]),
   ?assert(
      undefined =/= proplists:get_value(dns, Pf)
   ),
   ?assert(
      undefined =/= proplists:get_value(tcp, Pf)
   ),
   ?assert(
      undefined =/= proplists:get_value(ssl, Pf)
   ),
   ?assert(
      undefined =/= proplists:get_value(size, Pf)
   ),
   ?assert(
      undefined =/= proplists:get_value(chnk, Pf)
   ),
   ?assert(
      undefined =/= proplists:get_value(pckt, Pf)
   ).

http_test() ->
   ssl:start(),
   C = emc:c(runnable, emc_pf:new(emc_id, 0), hof_perf:net()),
   [Pf, {http, 302, _, _}] = C(["http://www.google.com"]),
   ?assert(
      undefined =/= proplists:get_value(dns, Pf)
   ),
   ?assert(
      undefined =/= proplists:get_value(tcp, Pf)
   ),
   ?assert(
      undefined =/= proplists:get_value(ttfb, Pf)
   ),
   ?assert(
      undefined =/= proplists:get_value(ttmr, Pf)
   ),
   ?assert(
      undefined =/= proplists:get_value(size, Pf)
   ),
   ?assert(
      undefined =/= proplists:get_value(chnk, Pf)
   ),
   ?assert(
      undefined =/= proplists:get_value(pckt, Pf)
   ).   

https_test() ->
   ssl:start(),
   C = emc:c(runnable, emc_pf:new(emc_id, 0), hof_perf:net()),
   [Pf, {http, 200, _, _}] = C(["https://www.google.com"]),
   ?assert(
      undefined =/= proplists:get_value(dns, Pf)
   ),
   ?assert(
      undefined =/= proplists:get_value(tcp, Pf)
   ),
   ?assert(
      undefined =/= proplists:get_value(ssl, Pf)
   ),
   ?assert(
      undefined =/= proplists:get_value(ttfb, Pf)
   ),
   ?assert(
      undefined =/= proplists:get_value(ttmr, Pf)
   ).
   %?assert(
   %   undefined =/= proplists:get_value(size, Pf)
   %),
   %?assert(
   %   undefined =/= proplists:get_value(chnk, Pf)
   %),
   %?assert(
   %   undefined =/= proplists:get_value(pckt, Pf)
   %).   


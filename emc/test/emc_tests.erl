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
-module(emc_tests).
-author(dmitry.kolesnikov@nokia.com).
-include_lib("eunit/include/eunit.hrl").


emc_seq_test() ->
   P = emc:seq([
      fun(X) -> [X + 5, X] end,
      fun(X) -> [X * 2, X] end
   ]),
   C = emc:c(emc_id, P),
   ?assert( 
      {[20, 10, 5], undefined} =:= C(5)
   ).
   
emc_seq_error_test() ->
   P = emc:seq([
      fun(X) -> [X + 5, X] end,
      fun(X) -> {error, not_supported} end,
      fun(X) -> [X * 2, X] end
   ]),
   C = emc:c(emc_id, P),
   ?assert( 
      {error, not_supported} =:= C(5)
   ).
   
emc_alt_test() ->
   P = emc:alt([
      fun
         (X) when X > 5 -> X - 5; 
         (_) -> {error, not_supported}
      end,
      fun
         (X) when X < 5 -> X + 5;
         (_) -> {error, not_supported}
      end
   ]),
   C = emc:c(emc_id, P),
   ?assert( 
      {[1], undefined} =:= C(6)
   ),
   ?assert( 
      {[6], undefined} =:= C(1)
   ),
   ?assert(
      {error, no_alt}  =:= C(5)
   ).
   
'emc_?_test'() ->
   P = emc:seq([
      fun(X) -> [X + 5, X] end,
      emc:'?'(fun(X) -> {error, not_supported} end),
      fun(X) -> [X * 2, X] end
   ]),
   C = emc:c(emc_id, P),
   ?assert( 
      {[20, 10, 5], undefined} =:= C(5)
   ).   
   
'emc_*_test'() ->
   P = emc:'*'(
      fun
         (10,X) -> {error, normal};
         (I, X) -> [I + 1, X + 1]
      end
   ),
   C = emc:c(emc_id, P),
   ?assert(
      {[10, 10], undefined} =:= C([0, 0])
   ).
   
emc_sub_pipe_test() ->
   P = emc:seq([
      fun(X) -> [X + 5, X] end,
      emc:seq([
         fun(X) -> X + 1 end,
         fun(X) -> X - 1 end
      ]),
      fun(X) -> [X * 2, X] end
   ]),
   C = emc:c(emc_id, P),
   ?assert( 
      {[20, 10, 5], undefined} =:= C(5)
   ).





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
-module(ek_reg_tests).
-author(dmitry.kolesnikov@nokia.com).
-include_lib("eunit/include/eunit.hrl").

start_test() ->
   {ok, _} = ek_reg:start_link().
   
register_test() ->
   ?assert(
      ok =:= ek_reg:register("urn:test", self())
   ).
   
register_twice_test() ->
   ?assert(
      badarg =:= (catch ek_reg:register("urn:test", self()) )
   ).
   
whereis_test() ->
   ?assert(
      self() =:= ek_reg:whereis("urn:test")
   ).
   
unregister_test() ->
   ?assert(
      ok =:= ek_reg:unregister("urn:test")
   ).
   
unregister_twice_test() ->
   ?assert(
      badarg =:= (catch ek_reg:unregister("urn:test"))
   ).

q_prep_test() ->
   ek_reg:register("http:/path", self()),
   ek_reg:register("urn:/path", self()),
   ek_reg:register("http://localhost:8080/path", self()).
   
q_scheme_test() ->
   R = ek_reg:q(
      fun(http) -> true; (_) -> false end,
      fun(_) -> true end,
      fun(_) -> true end
   ),
   ?assert(lists:member({http, undefined, <<"/path">>}, R)),
   ?assert(lists:member({http, <<"localhost:8080">>, <<"/path">>}, R)).
   
q_host_1_test() ->
   R = ek_reg:q(
      fun(_) -> true end,
      fun(undefined) -> true; (_) -> false end,
      fun(_) -> true end
   ),
   ?assert(lists:member({http, undefined, <<"/path">>}, R)),
   ?assert(lists:member({urn,  undefined, <<"/path">>}, R)).
   
q_host_2_test() ->
   R = ek_reg:q(
      fun(_) -> true end,
      fun(undefined) -> false; (_) -> true end,
      fun(_) -> true end
   ),
   ?assert(lists:member({http, <<"localhost:8080">>, <<"/path">>}, R)).
   
q_path_test() ->
   R = ek_reg:q(
      fun(_) -> true end,
      fun(_) -> true end,
      fun(<<"/path">>) -> true; (_) -> false end
   ),
   ?assert(lists:member({http, undefined, <<"/path">>}, R)),
   ?assert(lists:member({urn,  undefined, <<"/path">>}, R)),
   ?assert(lists:member({http, <<"localhost:8080">>, <<"/path">>}, R)).
   
   
registered_test() ->
   R = ek_reg:registered(),
   ?assert(lists:member({http, undefined, <<"/path">>}, R)),
   ?assert(lists:member({urn,  undefined, <<"/path">>}, R)).   
   
registered_scheme_test() ->
   R = ek_reg:registered(http),
   ?assert(lists:member({http, undefined, <<"/path">>}, R)).   
   
remote_test() ->
   R = ek_reg:remote(),
   ?assert(lists:member({http, <<"localhost:8080">>, <<"/path">>}, R)).
      
   
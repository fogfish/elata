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
-module(kvs_bucket_tests).
-author(dmitry.kolesnikov@nokia.com).
-include_lib("eunit/include/eunit.hrl").

basic_domain_test_() ->
   {
      setup,
      fun() -> 
         kvs_sys:construct([kvs_sys_ref]),
         kvs_sys:construct([kvs_sys_bucket])
      end,
      [
      { "Define domain", fun define/0},
      { "Lookup domain", fun lookup/0},
      { "Delete domain", fun delete/0},
      { "Undefined domain", fun undefined/0} 
      ]
   }.
   
%%
define() ->
   ?assert(
      ok =:= kvs_bucket:define(test, [{storage, kvs_sys}])
   ).
 
%%
lookup() ->
   ?assert(
      {ok, [{name, test}, {storage, kvs_sys}]} =:= kvs_bucket:lookup(test)
   ).

%%
delete() ->
   ?assert(
      ok =:= kvs_bucket:remove(test)
   ).
   
undefined() ->
   ?assert(
      {error, not_found} =:= kvs_bucket:lookup(test)
   ).
   
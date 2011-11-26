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
-module(kvs_fs_tests).
-author(dmitry.kolesnikov@nokia.com).
-include_lib("eunit/include/eunit.hrl").

kvs_fs_binary_test_() ->
   {
      setup,
      fun() ->
         kvs:start(),
         {ok, _} = kvs:new("kvs:test", [
            {storage, kvs_fs}, 
            {root, "/tmp/kvs"},
            {type, binary}
         ])
      end,
      fun(_) ->
         application:stop(kvs),
         timer:sleep(100),
         os:cmd("rm -R /tmp/kvs")
      end,
      [
      { "Put binary item", fun b_put/0},
      { "Has binary item", fun b_has/0},
      { "Get binary item", fun b_get/0},
      { "Put term   item", fun t_put/0},
      { "Has term   item", fun t_has/0},
      { "Get term   item", fun t_get/0}
      ]
   }.
   
kvs_fs_file_test_() ->
   {
      setup,
      fun() ->
         kvs:start(),
         {ok, _} = kvs:new("kvs:test", [
            {storage, kvs_fs}, 
            {root, "/tmp/kvs"},
            {type, file}
         ])
      end,
      fun(_) ->
         application:stop(kvs),
         timer:sleep(100),
         os:cmd("rm -R /tmp/kvs")
      end,
      [
      { "Put binary item", fun f_put/0},
      { "Has binary item", fun b_has/0},
      { "Get binary item", fun b_get/0}
      ]
   }.
   

%%
%%
b_put() ->
   Key = <<"file.bin">>,
   ?assert(
      ok =:= kvs:put("kvs:test", Key, <<"value">>)
   ),
   ?assert(
      filelib:is_file("/private/tmp/kvs" ++ key_to_stream(Key))
   ).
   
t_put() ->
   Key = <<"file.term">>,
   ?assert(
      ok =:= kvs:put("kvs:test", Key, {value})
   ),
   ?assert(
      filelib:is_file("/private/tmp/kvs" ++ key_to_stream(Key))
   ).   
   
f_put() ->
   Key = <<"file.bin">>,
   ?assert(
      ok =:= kvs:put("kvs:test", Key, <<"value">>)
   ),
   ?assert(
      filelib:is_file("/private/tmp/kvs/file.bin")
   ).   

%%
%%
b_has() ->
   Key1 = <<"file.bin">>,
   Key2 = <<"nokey">>,
   ?assert(
      true =:= kvs:has("kvs:test", Key1)
   ),
   ?assert(
      false =:= kvs:has("kvs:test", Key2)
   ).

t_has() ->
   Key1 = <<"file.term">>,
   Key2 = <<"nokey">>,
   ?assert(
      true =:= kvs:has("kvs:test", Key1)
   ),
   ?assert(
      false =:= kvs:has("kvs:test", Key2)
   ).   
   
%%
%%
b_get() ->
   Key1 = <<"file.bin">>,
   Key2 = <<"nokey">>,
   ?assert(
      {ok, <<"value">>} =:= kvs:get("kvs:test", Key1)
   ),
   ?assert(
      {error, not_found} =:= kvs:get("kvs:test", Key2)
   ).
   
%%
%%
t_get() ->
   Key1 = <<"file.term">>,
   Key2 = <<"nokey">>,
   ?assert(
      {ok, {value}} =:= kvs:get("kvs:test", Key1)
   ),
   ?assert(
      {error, not_found} =:= kvs:get("kvs:test", Key2)
   ).   
   
   
   
   
key_to_stream(Key) ->
   Hash = crypto:sha(term_to_binary(Key)),
   Hex  = [ integer_to_list(X, 16) || X <- binary_to_list(Hash) ],
   File = lists:append(Hex),
   "/" ++ lists:sublist(File, 2) ++ "/" ++ File.   
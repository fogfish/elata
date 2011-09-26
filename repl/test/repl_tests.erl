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
-module(repl_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
%% should this be included from somewhere
%% trouble using a record in ets, how to define matches?
-record(b_log, {
   ts,       %% timestamp as key
   bucket,   %% 
   key,      %%
   op,       %% operation: put or remove
   item      %% the actual item
}). 

%%
%% Unit test of replication
%%
repl_test_() ->
   {
      setup,
      fun setup/0,
      [
      { "Put    event", fun put/0},
      { "Update event", fun update/0},
      { "Remove event", fun remove/0},
      { "Godot event", fun godot/0}
      ]
   }.
   
setup() ->
   application:start(elata_kvs),
   application:start(elata_repl),
   timer:sleep(100),
   kvs_bucket:define(test_repl, [event, {storage, kvs_cache_sup}, {id, {attr, 1}}]).
   

put() ->   
   ?assert(
      ok =:= kvs:put(test_repl, a, {a, b, c})
   ),
   timer:sleep(400),
%%   error_logger:info_report(["ets match:", [ ets:match(repl_tslog, {'$1', test_repl, a, put, '$2'})] ]),
%%   error_logger:info_report(["ets match:", [ ets:match(repl_tslog, '$1')] ]),
   ?assert(
	[[{a,b,c}]] =:= ets:match(repl_tslog, {'_', test_repl, a, put, '$1'})
   ).   


update() ->   
   ?assert(
      ok =:= kvs:put(test_repl, a, {a, b, d})
   ),
   timer:sleep(400),
%%   error_logger:info_report(["ets match:", [ ets:match(repl_tslog, {'_', test_repl, a, put, '$1'})] ]),
%%   error_logger:info_report(["ets match:", [ ets:match(repl_tslog, '$1')] ]),
   ?assert(
	[[{a,b,c}], [{a,b,d}]] =:= ets:match(repl_tslog, {'_', test_repl, a, put, '$1'})
   ).   

   
remove() ->
   ?assert(
      ok =:= kvs:remove(test_repl, a)
   ),
   timer:sleep(400),
%%   error_logger:info_report(["ets match:", [ ets:match(repl_tslog, {'_', test_repl, a, remove, '$1'})] ]),
   ?assert(
	[[undefined]] =:= ets:match(repl_tslog, {'_', test_repl, a, remove, '$1'})
   ).

   
godot() ->
%% wait here to show maybe some events happening in the system...
   timer:sleep(1000),
   ?assert(
	[[undefined]] =:= ets:match(repl_tslog, {'_', test_repl, a, remove, '$1'})
   ).


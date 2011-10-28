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
-module(kvs_ht_tests).
-author(dmitry.kolesnikov@nokia.com).
-include_lib("eunit/include/eunit.hrl").

-define(SET1, [
   {key1, "value 1"},
   {key2, "value 2"},
   {set1, "set1"},
   {key3, "value 3"},
   {key4, "value 4"},
   {key5, "value 5"},
   {key6, "value 6"},
   {key7, "value 7"},
   {key8, "value 8"},
   {key9, "value 9"},
   {key10,"value 10"}
]).

-define(SET2, [
   {key1, "value 1"},
   {key2, "value 2"},
   {key3, "value 3"},
   {key4, "value 4"},
   {key5, "value 5"},
   {set2, "set2"},
   {key6, "value 6"},
   {key7, "value 7"},
   {key8, "value 8"},
   {key9, "value 9"},
   {key11,"value 11"}
]).

-define(SET3, [
   {key1, "value 1"},
   {key2, "value 2"},
   {set3, "set3"},
   {key3, "value 3"},
   {key4, "value 4"},
   {key5, "value 5"},
   {key6, "value 6"},
   {key7, "value 7"},
   {key8, "value 8"},
   {key9, "*value 9"},
   {key10,"*value 10"}
]).

recon_keyset_test() ->
   { A, B } = kvs_ht:reconsile(kvs_ht:new(?SET1), kvs_ht:new(?SET2)),
   ?assert(lists:member(set1,  A)),
   ?assert(lists:member(key10, A)),
   ?assert(lists:member(set2,  B)),
   ?assert(lists:member(key11, B)).

recon_value_test() ->
   { A, B } = kvs_ht:reconsile(kvs_ht:new(?SET1), kvs_ht:new(?SET3)),
   ?assert(lists:member(set1,  A)),
   ?assert(lists:member(key9,  A)),
   ?assert(lists:member(key10, A)),
   ?assert(lists:member(set3,  B)),
   ?assert(lists:member(key9,  B)),
   ?assert(lists:member(key10, B)).
   
recon_cold_test() ->
   { A, B } = kvs_ht:reconsile(kvs_ht:new(?SET1), kvs_ht:new()),
   ?assert([] =:= B),
   ?assert(lists:member(set1,  A)),
   ?assert(lists:member(key1,  A)),
   ?assert(lists:member(key2,  A)),
   ?assert(lists:member(key3,  A)),
   ?assert(lists:member(key4,  A)),
   ?assert(lists:member(key5,  A)),
   ?assert(lists:member(key6,  A)),
   ?assert(lists:member(key7,  A)),
   ?assert(lists:member(key8,  A)),
   ?assert(lists:member(key9,  A)),
   ?assert(lists:member(key10, A)).
   %error_logger:info_report([{'A', A}, {'B', B}]).

   



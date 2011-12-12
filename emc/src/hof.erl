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
-module(hof).
-author(dmitry.kolesnikov@nokia.com).


%%
%% High-order functions: Common
%%
-export([
   ok/0,
   ok/1,
   ok/2,
   ok/3,
   ok/4,
   bin_to_hex/1,
   hex_to_bin/1
]).

%%
%%
ok() ->
   {ok, []}.
ok(Y1) -> 
   {ok, [Y1]}.
ok(Y1, Y2) ->
   {ok, [Y1, Y2]}.
ok(Y1, Y2, Y3) ->
   {ok, [Y1, Y2, Y3]}.
ok(Y1, Y2, Y3, Y4) ->
   {ok, [Y1, Y2, Y3, Y4]}.   


%%   
%% bin_to_hex(Bin) -> Hex
%%    Bin = binary()
%%    Hex = list() hexdec 
bin_to_hex(Bin) ->
   bin_to_hex(Bin, "").
   
bin_to_hex(<<>>, Acc) ->
   Acc;
bin_to_hex(<<X:8, T/binary>>, Acc) ->  
   bin_to_hex(T, Acc ++ [to_hex(X div 16), to_hex(X rem 16)]).
   
to_hex(X) when X < 10 ->
   $0 + X;
to_hex(X) ->
   $a + (X - 10).

%%
%% hex_to_bin(Hex) -> Bin
%%    Hex = list()
%%    Bin = binary()
hex_to_bin(Hex) ->
   hex_to_bin(Hex, <<>>).

hex_to_bin([], Acc) ->
   Acc;
hex_to_bin([H, L | T], Acc) ->
   V = to_int(H) * 16 + to_int(L),
   hex_to_bin(T, <<Acc/binary, V>>).

to_int(C) when C >= $a, C =< $f ->
   10 + (C - $a);
to_int(C) when C >= $0, C =< $9 ->
   C - $0.   

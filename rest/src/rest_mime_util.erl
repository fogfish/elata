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
-module(rest_mime_util).
-author(dmitry.kolesnikov@nokia.com).

-export([
   octet_stream/1,
   text_plain/1,
   erlang_base64/1
]).


%% encode/decode term to/from octet-stream
octet_stream({encode, Term}) ->
   term_to_binary(Term);
octet_stream({decode, Msg}) ->
   binary_to_term(Msg).
   
%% encode/decode term to/from text/plain
text_plain({encode, Term}) ->
   lists:flatten(io_lib:format("~p.", [Term]));
text_plain({decode, Msg}) when is_binary(Msg) ->
   text_plain({decode, binary_to_list(Msg)});
text_plain({decode, Msg}) ->
   {ok, Scanned, _} = erl_scan:string(Msg),
   {ok, T} = erl_parse:parse_term(Scanned),
   T.
   
%% encode/decode term to/from application/erlang-base64
erlang_base64({encode, Term}) ->
   base64:encode(
      term_to_binary(Term)
   );
erlang_base64({decode, Msg}) ->
   Bin = base64:decode(Msg),
   binary_to_term(Bin).
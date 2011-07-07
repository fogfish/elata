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
-module(tsa_filter).
-author(dmitry.kolesnikov@nokia.com).

-export([
   to_value/1,
   moving_mean/2,
   moving_std/2,
   max/2,
   min/2
]).

to_value(F) ->
   erlang:element(2, F).

%%   
%% 
moving_mean({filter, _Value, _Size}, eof) ->
   {filter, 0, 0};

moving_mean(undefined, Raw) ->
   {filter, Raw, 1};
   
moving_mean({filter, Value, Size}, Raw) ->
   NSize  = Size + 1,
   NValue = erlang:round( ((NSize - 1) * Value + Raw) / NSize ),
   {filter, NValue, NSize}.
   
%%   
%% 
moving_std({filter, _Value, _Size, _Var, _Mean}, eof) ->
   {filter, 0, 0, 0, 0};

moving_std(undefined, Raw) ->
   {filter, Raw, 1, 0, Raw};
   
moving_std({filter, _Value, Size, Var, Mean}, Raw) ->
   NSize  = Size + 1,
   NMean  = erlang:round(
      ((NSize - 1) * Mean + Raw) / NSize 
   ),
   NVar   = erlang:round(
      (NSize - 1) / NSize * Var + NSize / ( (NSize - 1) * (NSize - 1) ) * (Raw - NMean) * (Raw - NMean)
   ),
   NValue = erlang:round( math:sqrt(NVar) ),
   {filter, NValue, NSize, NVar, NMean}.   
   
%%
%%
max({filter, _Value}, eof) ->
   {filter, 0};
   
max(undefined, Raw) ->
   {filter, Raw};
   
max({filter, Value}, Raw) ->
   if
      Value >= Raw ->
         {filter, Value};
      Value <  Raw ->
         {filter, Raw}
   end.
   
%%
%%
min({filter, _Value}, eof) ->
   {filter, 0};
   
min(undefined, Raw) ->
   {filter, Raw};
   
min({filter, Value}, Raw) ->
   if
      Value >= Raw ->
         {filter, Raw};
      Value <  Raw ->
         {filter, Value}
   end.

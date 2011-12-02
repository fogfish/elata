%%
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
-module(em_pipe).
-author(sergey.boldyrev@nokia.com).
-author(dmitry.kolesnikov@nokia.com).


%%
%% Pipe monad
%%
-export([
   unit/1,
   bind/2         
]).

%%
%% debug macro
-ifdef(DEBUG).
-define(DEBUG(M), error_logger:info_report([{?MODULE, self()}] ++ M)).
-else.
-define(DEBUG(M), true).
-endif.

%%
%%
unit(X) when is_list(X) ->
   fun() ->  X  end;
unit(X) ->
   fun() -> [X] end.

   
%%
%%
bind(X, HF) ->
   Info      = erlang:fun_info(HF),
   Arity     = proplists:get_value(arity,  Info),
   %error_logger:info_report([{hof, HF}, {a, Arity}, {x, X}, {len, length(X)}]),
   %try 
      if
         length(X) > Arity -> 
            {NX, T} = lists:split(Arity, X),
            Y = u(erlang:apply(HF, NX)) ++ T,
            ?DEBUG([{hof, HF}, {in, X}, {out, Y}, {split, T}]),
            Y;
         true              -> 
            Y = u(erlang:apply(HF, X)),
            ?DEBUG([{hof, HF}, {in, X}, {out, Y}]),
            Y
      end.
   %catch
   %   Err -> 
   %      ?DEBUG([{hof, HF}, {in, X}, {err, Err}]),
   %      X
   %end.

   
u(X) when is_list(X) ->   
   X;
u(X) ->
   [X].

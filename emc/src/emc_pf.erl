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
-module(emc_pf, [M]).
-author(sergey.boldyrev@nokia.com).
-author(dmitry.kolesnikov@nokia.com).

%%
%% Performance Monad
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
unit(X) ->
   {m, Xm, MX} = M:unit(X),
   {m, Xm, [{?MODULE, []} | MX]}.

%%
%%
bind({m, X, MX}, HOF) ->
   ?DEBUG([{monad, {?MODULE, M}},{hof, HOF}, {x, X}]),
   case timer:tc(M, bind, [{m, X, MX}, HOF]) of
      {Time, {ok, {m, Y, MY}}} ->
         Info      = erlang:fun_info(HOF),
         Mod       = proplists:get_value(module, Info),
         Name      = proplists:get_value(name,   Info),
         Arity     = proplists:get_value(arity,  Info),
         Id        = list_to_binary(atom_to_list(Mod) ++ ":" ++ atom_to_list(Name) ++ "/" ++ integer_to_list(Arity)),
         MYself    = [ {Id, Time} | proplists:get_value(?MODULE, MY)],
         MYglob    = [ MYself | proplists:delete(?MODULE, MY)],
         {ok, {m, Y, MYglob}};
      {_Time, Error}  -> Error
   end.


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
-module(emc).
-author(sergey.boldyrev@nokia.com).
-author(dmitry.kolesnikov@nokia.com).

%%
%% Erlang Monadic Computation framework
%%
%% The computation is function, it takes input and returns output
%% fun(x_1, x_2, ..., x_n) -> [y_1, y_2, ..., y_m]
%% 
%% A monad is a triple (M,unit,bind) consisting of a type constructor M 
%% and two operations of the given polymorphic types.
%%
%% unit :: x → M x               
%% turn a value into the computation that returns that value and does 
%% nothing else
%%
%% bind :: M x → (x → M y) → M y 
%% apply a function of type x → M y to a computation of type M x.
%%
%%

-export([
   do/1,
   do/2
]).

%%
%% debug macro
-ifdef(DEBUG).
-define(DEBUG(M), error_logger:info_report([{?MODULE, self()}] ++ M)).
-else.
-define(DEBUG(M), true).
-endif.

%%
%% do(Seq) -> Fun({M, State}) -> Fun(X) -> Result
%%
%% The "do" operator wraps a computation pipeline of high order functions
%% into monadic computation. The result is a monadic type constructor.
%% The type constructor parametrizes the pipeline with a particular 
%% monad instance. As the result, is the computation pipiline that 
%% can be evaluated agains input values
do(HOFs) ->
   fun
     (M) ->
        fun(X) ->
           lists:foldl(
              fun(HF,X1) -> M:bind(X1, HF) end, 
              erlang:apply(M:unit(X), []), 
              HOFs
           )
        end
   end.

do(M, HOFs) ->
   C = do(HOFs),
   C(M).
   

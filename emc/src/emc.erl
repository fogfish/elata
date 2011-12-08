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
%% either successfull or error
%% fun(x_1, x_2, ..., x_n) -> {ok, [y_1, y_2, ..., y_m]} | {error, ...}
%%
%% Computations are chained to form a pipleline, output of first
%% function in the input to next sibling and so, on. The failure 
%% interupts the pipeline
%%
%% There are number of primitives to handle pipeline flow control:
%%   - seq(...) evaluates computations sequentially.
%%   - alt(...) alternative computation, next computation is evaluated 
%%              if previous is failed.
%%   - '?'(...) (null or one)  evaluates computation once, any errors are terminated
%%   - '*'(...) (null or many) evaluates computation until it fails, erros are terminated
%% 
%% Computations by it self are wrapped by Monads, that acts as 
%% programmable comma and influences flow control.
%%
%% A monad is a triple (M,unit,bind) consisting of a type constructor M 
%% and two operations of the given polymorphic types.
%%
%% unit :: x → M x               
%% turn a value into the computation that returns that value and does 
%% nothing else. (Wraps value into Monadic-type)
%%
%% bind :: M x → (x → M y) → M y 
%% apply a function of type x → M y to a computation of type M x.
%%

-export([
   c/2,
   c/3,
   seq/1,
   alt/1,
   '?'/1,
   '*'/1,
   '='/1
]).

%%
%% debug macro
-ifdef(DEBUG).
-define(DEBUG(M), error_logger:info_report([{?MODULE, self()}] ++ M)).
-else.
-define(DEBUG(M), true).
-endif.

%% internal record, encapsulates a context
-record(emc, {
   m, % monad assotiated with pipeline
   s  % state, monadic type {m, input/output values, m state}
}).

%%
%% compiles a pipeline into manadic computation
c(M, P) ->
   fun(X) ->
      case P(#emc{m = M, s = M:unit(X)}) of
         {ok, {emc, _, {m, Y, MY}}} -> {Y, MY};
         Err -> Err
      end
   end.

c(runnable, M, P) ->
   fun(X) ->
      case P(#emc{m = M, s = M:unit(X)}) of
         {ok, {emc, _, {m, Y, _MY}}} -> Y;
         Err -> Err
      end
   end;
c(monad, M, P) ->
   fun(X) ->
      case P(#emc{m = M, s = M:unit(X)}) of
         {ok, {emc, _, {m, _Y, MY}}} -> MY;
         Err -> Err
      end
   end.
   

%%   
%% seq(...) -> Fun
%%
seq(Seq) ->
   fun(X) when is_record(X, emc) -> '_seq'(Seq, X) end.

%%
%% alt(...) -> Fun
%%
alt(Seq) ->
   fun(X) when is_record(X, emc) -> '_alt'(Seq, X) end.

%%
%% '?'(...) -> Fun
%%
'?'(Seq) ->
   fun(X) when is_record(X, emc) -> '_?'(Seq, X) end.

%%
%% '*'(...) -> Fun
%%
'*'(Seq) ->
   fun(X) when is_record(X, emc) -> '_*'(Seq, X) end.

%%
%% Pseudo operand to unpack monad state into stack
%%
'='(M) -> 
   fun(X) when is_record(X, emc) -> '_='(M, X) end.
   
%%
%% do(Seq) -> Fun({M, State}) -> Fun(X) -> Result
%%
%% The "do" operator wraps a computation pipeline of high order functions
%% into monadic computation. The result is a monadic type constructor.
%% The type constructor parametrizes the pipeline with a particular 
%% monad instance. As the result, is the computation pipiline that 
%% can be evaluated agains input values
%do(HOFs) ->
%   fun
%     (M) ->
%        fun(X) ->
%           lists:foldl(
%              fun(HF,X1) -> M:bind(X1, HF) end, 
%              erlang:apply(M:unit(X), []), 
%              HOFs
%           )
%        end
%   end.
%
%do(M, HOFs) ->
%   C = do(HOFs),
%   C(M).
%   
   
%%-------------------------------------------------------------------
%%
%% Private
%%
%%-------------------------------------------------------------------

'_seq'([], S) ->
   {ok, S};
'_seq'([F | Seq], S) ->  
   case eval(F, S) of
      {ok, NS}  -> '_seq'(Seq, NS);
      Err       -> Err
   end.

'_alt'([], S) ->
   {error, no_alt};
'_alt'([F | Seq], S) ->
   case eval(F, S) of
      {error, _} -> '_alt'(Seq, S);
      {ok, Snew} -> {ok, Snew}
   end.
   
'_?'(F, S) ->
   case eval(F, S) of
      {error, _} -> {ok, S};
      {ok, Snew} -> {ok, Snew}
   end.
   
'_*'(F, S) ->
   case eval(F, S) of
      {error, _} -> {ok, S};
      {ok, Snew} -> '_*'(F, Snew)
   end.

'_='(M, #emc{s = {m, X, MX}} = S) ->
   Mx = proplists:get_value(M, MX),
   {ok,
      S#emc{
         s = {m, [Mx | X], MX}
      }
   }.
    
%%
%% evaluates a computation
eval(F, #emc{m = M, s = {m, X, MX}} = S) ->
   case fun_type(F) of
      {emc, _} -> 
         ?DEBUG([{eval, emc}, {'fun', erlang:fun_info(F)}, {state, S}]),
         F(S);
      {hof, Arity} -> 
         ?DEBUG([{eval, hof}, {'fun', erlang:fun_info(F)}, {state, S}]),
         if 
            length(X)  >  Arity ->
               {Xh, Xt} = lists:split(Arity, X),
               pack(M:bind({m, Xh, MX}, F), S, Xt);
            length(X)  <  Arity ->
               Xn = X ++ lists:duplicate(Arity - length(X), undefined),
               pack(M:bind({m, Xn, MX}, F), S, []);
            length(X) =:= Arity ->
               pack(M:bind({m, X, MX},  F), S, [])
         end
   end.
   
%%
%% resolve type of computation either hof | emc and arity
fun_type(F) ->
   Info  = erlang:fun_info(F),
   Arity = proplists:get_value(arity,  Info),
   case proplists:get_value(module, Info) of
      emc -> {emc, Arity};
      _   -> {hof, Arity}
   end.
   
%%
%% Yf -> results of computation
%% Ym -> results of monad
%% Xt -> reminder of input
%% pack output results into context
pack({ok, {m, Y, undefined}}, #emc{s = {m, _, MX}} = S, Xt) ->
   {ok,
      S#emc{
         s = {m, Y ++ Xt, MX}
      }
   };

pack({ok, {m, Y, MY}}, #emc{s = {m, _, _MX}} = S, Xt) ->
   {ok,
      S#emc{
         s = {m, Y ++ Xt, MY}
      }
   };

pack(Err, _S, _Xt) ->
   Err.

   

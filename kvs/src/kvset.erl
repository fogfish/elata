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
-module(kvset).
-author(dmitry.kolesnikov@nokia.com).

%%
%% Nested set of {k, v} - pairs
%%

-export([
   new/1,
   name/1,
   set/3,
   get/2,
   get/3,
   keys/1,
   keys/2,
   append/3
]).

%% create a new set instance
new(Name) ->
   {kvset, Name, []}.
   
%% get name of set   
name({kvset, Name, _}) ->
   Name.

%% add {k,v} pair to set  
set(Key, Val, {kvset, Name, Set}) when is_list(Key) ->   
   {kvset, Name, add_to_set(Key, Val, Set)};
set(Key, Val, {kvset, Name, Set}) ->
   {kvset, Name, add_to_set([Key], Val, Set)}.

%% get {Key, Val} pair
get(Key, Set) ->
   get(Key, Set, false).

get(Key, {kvset, _Name, Set}, Default) when is_list(Key) ->
   get_from_set(Key, Set, Default);
get(Key, {kvset, _Name, Set}, Default) ->
   get_from_set([Key], Set, Default).
   
   
%%   
keys({kvset, _Name, Set}) ->
   lists:map(
      fun({Key, _}) -> Key end,
      Set
   ).

keys(Key, {kvset, _Name, Set}) when is_list(Key) ->   
   lists:map(
      fun({X, _}) -> X end,
      get_from_set(Key, Set, [])
   );

keys(Key, {kvset, _Name, Set}) ->
   lists:map(
      fun({X, _}) -> X end,
      get_from_set([Key], Set, [])
   ).
   

%%    
append(Key, {kvset, _Name1, Src}, {kvset, Name, Dst}) when is_list(Key)->
   {kvset, Name, replace_to_set(Key, Src, Dst)};
   
append(Key, {kvset, _Name1, Src}, {kvset, Name, Dst}) ->
   {kvset, Name, replace_to_set([Key], Src, Dst)}.
   
%%%------------------------------------------------------------------   
%%%
%%% Private Functions
%%%
%%%------------------------------------------------------------------   

%
add_to_set([Key], Val, Set) ->
   NVal = case lists:keyfind(Key, 1, Set) of
      false ->
         Val;
      {_K, List} when is_list(List) ->
         [Val | lists:delete(Val, List)];
      {_K, V}    when V =/= Val ->
         [Val, V];
      _ ->
         Val
   end,
   lists:keystore(Key, 1, Set, {Key, NVal});   
   
add_to_set([Key | T], Val, Set) ->
   NSubSet = case lists:keyfind(Key, 1, Set) of
      false         ->
         add_to_set(T, Val, []);
      {Key, SubSet} ->
         add_to_set(T, Val, SubSet)
   end,
   lists:keystore(Key, 1, Set, {Key, NSubSet}).

%
get_from_set([Key], Set, Default) ->
   case lists:keyfind(Key, 1, Set) of
      false      ->
         Default;
      {Key, Val} ->
         Val
   end;
   
get_from_set([Key | T], Set, Default) ->
   case lists:keyfind(Key, 1, Set) of
      false      ->
         Default;
      {Key, SubSet} ->
          get_from_set(T, SubSet, Default)  
   end.
   
%
replace_to_set([Key], Val, Set) ->
   lists:keystore(Key, 1, Set, {Key, Val});   
   
replace_to_set([Key | T], Val, Set) ->
   NSubSet = case lists:keyfind(Key, 1, Set) of
      false         ->
         replace_to_set(T, Val, []);
      {Key, SubSet} ->
         replace_to_set(T, Val, SubSet)
   end,
   lists:keystore(Key, 1, Set, {Key, NSubSet}).


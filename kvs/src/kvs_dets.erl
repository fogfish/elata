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
-module(kvs_dets).
-behaviour(gen_kvs).
-author(dmitry.kolesnikov@nokia.com).
-include_lib("stdlib/include/qlc.hrl").

%%
%%  On-disk category (uses DETS internally)
%%

-export([
   start_link/1,
   new/1,
   % gen_kvs
   put/3,
   has/2,
   get/2,
   remove/2,
   map/2,
   fold/3 
]).

%%
%% debug macro
-ifdef(DEBUG).
-define(DEBUG(M), error_logger:info_report([{?MODULE, self()}] ++ M)).
-else.
-define(DEBUG(M), true).
-endif.


%%
%% On-disk key/val storage
%%    Storage specific options
%%       {file, ...}
start_link(Spec) ->
  gen_kvs:start_link(Spec).
  
new(Spec) ->
   File = proplists:get_value(file,  Spec),
   Cat  = proplists:get_value(uri,   Spec),
   case filelib:is_file(File) of
      true  -> 
         ok;
      false ->
         % create dets tables
         dets:open_file(noname, [{file, File}]),
         dets:close(noname)
   end,
   dets:open_file(File).

%%%------------------------------------------------------------------   
%%%
%%% gen_kvs
%%%
%%%------------------------------------------------------------------

%%
%%
put(Key, Val, Ref) ->
   dets:insert(Ref, {Key, Val}).

%%
%%
has(Key, Ref) ->
   case dets:lookup(Ref, Key) of
      [Val] -> true;
      _     -> false
   end.   
   
%%
%%
get(Key, Ref) ->
   case dets:lookup(Ref, Key) of
      [{Key, Val}] -> {ok, Val};
      []           -> {error, not_found};
      Err          -> Err
   end.    
   
%%
%%
remove(Key, Ref) ->  
   dets:delete(Ref, Key).

%%
%%
map(Fun, Ref) ->
   Map = fun({K, V}) -> Fun(K, V) end,
   Q = qlc:q([ Map(X) || X <- dets:table(Ref)]),
   qlc:e(Q). 

%%   
%%
fold(Acc, Fun, Ref) ->
   Fold = fun({K, V}, A) -> Fun(K, V, A) end,
   Q = qlc:q([ X || X <- dets:table(Ref)]),
   qlc:fold(Fold, Acc, Q).   

%%%------------------------------------------------------------------   
%%%
%%% Private functions
%%%
%%%------------------------------------------------------------------


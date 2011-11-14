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
-module(kvs_ht).
-author(dmitry.kolesnikov@nokia.com).

%%%
%%% Hash-tree (Merkle tree), used to synchronize buckets:
%%%
%%% Synchronization scheme is build with following assumptions
%%%  - network data transfer is more expensive the local computation
%%%  - multiple network rtt of message << MTU is acceptable 
%%%  - keyspace transfer is more expensive then progressive reconsilation
%%%  - diff between keyspace is not "big"
%%%
%%% Reconsilation schema  
%%%  1. Depth <- tree root
%%%  2. Peer A: Send Tree signature at Depth to remote peer B
%%%  3. Peer B: Calulates Tree signature at same depth,
%%%             calculate difference and send it back to peer A
%%%  4. Peer A: Removes duplicated node at Depth, decrement Depth and goto 2 
%%%

-export([
   new/0,
   new/1,
   new/2,
   depth/1,
   insert/3,
   filter/2,
   keys/1,
   hash/2,
   diff/2
]).


-record(ht, {
   depth,  % node depth 0 - leaf, log_C(N) - root
   size,   % number of inner elements  
   hash,   % hash
   inner   % inner element(s)
}).
-define(CAPACITY, 2).


%%%
%%% Builds a new tree
new() ->
   #ht{
      depth  = 1, 
      size   = 0,
      inner  = []
   }.   
   
new([{Key, Item} | R]) ->
   new(R, insert(Key, Item, new())).
   
new([], T) ->
  T;
new([{Key, Item} | R], T) ->
   new(R, insert(Key, Item, T)).
   
%%
%% return depth
depth(#ht{depth = Depth}) ->
   % tree
   Depth; 
depth({hash, Depth, _}) ->
   % hash
   Depth.
   
   
%%%
%%% Insert
insert(Key, Item, T) ->
   case ht_insert(new_leaf(Key, Item), T) of
      {NewT, OldT} ->
         add_node([NewT, OldT], new_node(T#ht.depth + 1));
      NewT         ->
         NewT
   end.
   
ht_insert(Leaf, #ht{depth = Depth, inner = []} = T) ->
   % list of inner nodes is empty let's inject leaf
   if 
      Depth =:= 1 -> 
         add_node(Leaf, T);
      true        -> 
         add_node(
            ht_insert(Leaf, new_node(Depth - 1)),
            T 
         )   
   end;
ht_insert(Leaf, #ht{depth = 1} = T) ->
   case has_capacity(T) of
      true  -> add_node(Leaf, T);
      false -> {ht_insert(Leaf, new_node(T#ht.depth)), T}
   end;   
   
ht_insert(Leaf, T) ->
   case has_capacity(T) of
      true  ->
         [ Inner | SubT ] = T#ht.inner,
         case ht_insert(Leaf, Inner) of
            {NewInn, OldInn} ->
               add_node([NewInn, OldInn | SubT], T);
            NewInn           ->
               add_node([NewInn | SubT], T)
         end;
      false ->
         % no capacity do split
         {ht_insert(Leaf, new_node(T#ht.depth)), T}
   end.

%%%
%%% Removes listed nodes
filter(undefined, T) ->
   T;
filter({hash, Depth, Hashes}, T) ->
   if
      T#ht.depth >= Depth ->
         ht_filter(Depth, Hashes, T);
      true ->
         T
   end.

ht_filter(Depth, Hashes, #ht{depth = Tdepth, hash = Hash} = T) when Tdepth =:= Depth ->
   case lists:member(Hash, Hashes) of
      true  -> undefined;
      false -> T
   end;
   
ht_filter(Depth, Hashes, #ht{inner = Inner} = T) ->
   T#ht{
      inner = lists:foldl(
         fun(SubT, List) -> 
            case ht_filter(Depth, Hashes, SubT) of
               undefined -> List;
               Node      -> [Node | List]
            end
         end,
         [],
         Inner
      )
   }.

%%%   
%%% Return list of keys
keys(undefined) ->
   [];
keys(T) ->   
   ht_keys(T, []).
      
ht_keys(#ht{depth = 0, inner = Key}, Acc)  ->  
   [Key | Acc];
ht_keys(#ht{inner=Inner}, Acc) ->
   lists:foldl(
      fun(SubT, A) -> ht_keys(SubT, A) end,
      Acc,
      Inner
   ).   
      
   
%%%   
%%% Return hashes of tree at depth
%%% {hash, Depth, [Hashes]}
hash({hash, Depth, _}, T) ->
   hash(Depth, T);
hash(-1,    T) ->
   {hash, -1, []};
hash(Depth, undefined) ->
   {hash, Depth, []};
hash(Depth, T) ->
   if 
      T#ht.depth >= Depth ->
         {hash, Depth, ht_hash(Depth, T, [])};
      true ->
         {hash, Depth, []}
   end.
      
ht_hash(Depth, #ht{depth = Tdepth, hash = undefined}, Acc) when Tdepth =:= Depth ->
   Acc;
ht_hash(Depth, #ht{depth = Tdepth, hash = Hash}, Acc) when Tdepth =:= Depth  ->  
   [Hash | Acc];
ht_hash(Depth, #ht{inner=Inner}, Acc) ->
   lists:foldl(
      fun(SubT, A) -> ht_hash(Depth, SubT, A) end,
      Acc,
      Inner
   ).   
   
%%%
%%% Compares tree hashes, return hash tuple of intersection A and B
%%% 
diff({hash, DA, SA}, {hash, DB, SB}) when DA =:= DB ->
   L = lists:foldl(
      fun(X, Acc) -> 
         case lists:member(X, SA) of
            true  -> Acc ++ [X];
            false -> Acc
         end
      end,
      [],
      SB
   ),
   {hash, DA, L};
   
diff({hash, DA, _}, {hash, DB, _}) ->
   {hash, erlang:min(DA, DB), []};

   
%%
%% local tree(s) reconsilation
%% {unique at A, unique at B}
diff(A, B) when is_record(A, ht) and is_record(B, ht) ->
   ht_diff(
      erlang:min(kvs_ht:depth(A) - 1, kvs_ht:depth(B) - 1), 
      A, 
      B
   ).

ht_diff(-1,     A, B) ->
   {kvs_ht:keys(A), kvs_ht:keys(B)};
ht_diff(Depth, A, B) ->
   I = kvs_ht:diff(
      kvs_ht:hash(Depth, A), 
      kvs_ht:hash(Depth, B)
   ),
   ht_diff(Depth - 1,
      kvs_ht:filter(I, A),
      kvs_ht:filter(I, B)
   ).

   
%%%------------------------------------------------------------------
%%%
%%% Private 
%%%
%%%------------------------------------------------------------------

%%%
%%% hash function
hashf(X) ->
   crypto:sha(term_to_binary(X)).

%%%
%%%
new_leaf(Key, Item) ->
   #ht{
      depth = 0,
      size  = 1,
      hash  = hashf({Key, Item}),
      inner = Key
   }.

%%%
%%%
new_node(Depth) ->
   #ht{
      depth = Depth,
      size  = 0,
      hash  = undefined,
      inner = []
   }.
   
%%%
%%%
add_node(Inner, Node) when is_list(Inner) ->
   HashList = lists:map(
      fun(X) -> X#ht.hash end,
      Inner
   ),
   Size = lists:foldl(
      fun(X, Acc) -> Acc + X#ht.size end,
      0,
      Inner
   ),
   Node#ht{
      size  = Size,
      hash  = hashf(HashList),
      inner = Inner
   };


add_node(Inner, Node) when is_record(Inner, ht) ->
   add_node([Inner | Node#ht.inner], Node).


%%%
%%% check if node has a capacity
%%%    each node keeps depth & size (nbr of leaf nodes)
%%%    Node have capacity if log(size) / log(capacity) < depth
has_capacity(#ht{size = Size}) when Size =:= 0 ->
   true;
has_capacity(#ht{size = Size, depth = Depth}) ->
   FF = erlang:trunc( math:log(Size) / math:log(?CAPACITY) ),
   if
      FF < Depth -> true;
      true       -> false 
   end.

   
  
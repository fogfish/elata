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
-module(kvs).
-author(dmitry.kolesnikov@nokia.com).

%%%
%%% Active Key/Value store provides unified interface for key-value storage.
%%% Items are allocated to buckets. 
%%%

%%% TODO:
%%% put(Cat, List) -> bulk insert of Key/Val pairs

-export([
   start/0,
   start/1,
   new/2,
   drop/1,
   put/3,
   get/2,
   get/3,
   has/2,
   remove/2,
   map/2,
   fold/3
]).

%%%
%%% Start kvs application
start() ->
   kvs:start([]).

start(Config) ->
   lists:foreach(
      fun
         ({K, V}) -> application:set_env(kvs, K, V);
         (K)      -> application:set_env(kvs, K, true)
      end,
      Config
   ),
   {file, Module} = code:is_loaded(?MODULE),
   AppFile = filename:dirname(Module) ++ "/" ++ atom_to_list(?MODULE) ++ ".app",
   {ok, [{application, _, List}]} = file:consult(AppFile), 
   Apps = proplists:get_value(applications, List, []),
   lists:foreach(
      fun(X) -> application:start(X) end,
      Apps
   ),
   application:start(?MODULE).


%%%
%%% Define a new Key/Value category
%%%
%%% new(Cat, Opts) -> {ok, Pid} | {error, ...}
%%%    Cat  = URI (scheme kvs)
%%%    Opts = [opt]
%%%       {storage, Module} plugin implementation of gen_kvs_bucket
%%%       {getter,     Fun} Item getter function   Fun(Attr, Item) -> value 
%%%       event             events are fired when bucket is changed
new({kvs, undefined, _} = Cat, Opts) ->
   case ek:whereis(Cat) of
      undefined ->
         Spec = [{uri, Cat} | Opts],
         % Note: two-phase constructor is used to avoid 
         %       deadlock within supervisor when category
         %       internally creates another category
         {ok, Pid} = kvs_sup:start_category(Spec),
         ok = gen_kvs:new(Pid),
         {ok, Pid};
      Pid       ->
         {error, {already_exists, Pid}}
   end; 
new(Cat, Opts) when is_list(Cat) orelse is_binary(Cat) ->
   new(ek_uri:new(Cat), Opts).

   
%%%
%%% Drop a Key/Value category
%%%
%%% drop(Cat) -> ok
drop({kvs, undefined, _} = Cat) ->
   case ek:whereis(Cat) of
      undefined -> ok;
      Pid       -> {error, not_implemented} % TODO: implement drop
   end;
drop(Cat) when is_list(Cat) orelse is_binary(Cat) ->
   drop(ek_uri:new(Cat)).
   
   
%%%
%%% put(Cat, Key, Item) -> ok | {error, ...}
%%%    Cat = Uri
%%%    Val = item to store
%%%    Key = unique item identity (identity function over Item)
%%% 
%%% stores value into category
put({kvs, undefined, _} = Cat, Key, Val) when Key =/= undefined, 
                                              Val =/= undefined ->
   % stores into local passive category
   case ek:whereis(Cat) of
      undefined -> 
         {error, no_category};
      Pid       ->
         gen_kvs:put(type(Cat), Pid, Cat, Key, Val)
   end;
   
put({_, _, _} = Cat, Key, Val) when Key =/= undefined, 
                                    Val =/= undefined ->
   % stores into remote category
   gen_kvs:put(remote, undefined, Cat, Key, Val);
   
put(Cat, Key, Val) when Cat =/= undefined, 
                        Key =/= undefined, 
                        Val =/= undefined ->
   kvs:put(ek_uri:new(Cat), Key, Val).
   

%%%
%%% has(Cat, Key) -> true | false
%%%    Cat = Uri
%%%    Key = unique item identity
%%%
%%% check key in the category
has({kvs, undefined, _} = Cat, Key) when Key =/= undefined ->
   % check local passive category
   case ek:whereis(Cat) of
      undefined -> 
         {error, no_category};
      Pid       ->
         gen_kvs:has(type(Cat), Pid, Cat, Key)
   end;

has({_, _, _} = Cat, Key) when Key =/= undefined ->
   % check remote category
   gen_kvs:has(remote, undefined, Cat, Key);
   
has(Cat, Key) when Cat =/= undefined, 
                   Key =/= undefined ->
   kvs:has(ek_uri:new(Cat), Key).   



%%%
%%% get(Cat, Key) -> {ok, Item} | {error, ...}
%%%    Cat = Uri
%%%    Key = unique item identity
%%%
%%% retrives key from category
get({kvs, undefined, _} = Cat, Key) when Key =/= undefined ->
   % check local passive category
   case ek:whereis(Cat) of
      undefined -> 
         {error, no_category};
      Pid       ->
         gen_kvs:get(type(Cat), Pid, Cat, Key)
   end;
   
get({_, _, _} = Cat, Key) when Key =/= undefined ->
   % check remote category
   gen_kvs:get(remote, undefined, Cat, Key);
   
get(Cat, Key) when Cat =/= undefined, 
                   Key =/= undefined ->
   kvs:get(ek_uri:new(Cat), Key).   

get(Cat, Key, Default) ->
   case kvs:get(Cat, Key) of
      {error, not_found} -> {ok, Default};
      R -> R
   end.
   
   
%%%
%%% remove(Cat, Key) -> ok | {error, ...}
%%%    Cat  = Uri
%%%    Key  = unique item identity
%%%
%%% removes keys from bucket
remove({kvs, undefined, _} = Cat, Key) when Key =/= undefined ->
   % check local passive category
   case ek:whereis(Cat) of
      undefined -> 
         {error, no_category};
      Pid       ->
         gen_kvs:remove(type(Cat), Pid, Cat, Key)
   end;

remove({_, _, _} = Cat, Key) when Key =/= undefined ->
   % check remote category
   gen_kvs:remove(remote, undefined, Cat, Key);
   
remove(Cat, Key) when Cat =/= undefined, 
                   Key =/= undefined ->
   kvs:remove(ek_uri:new(Cat), Key).   
 
%%%
%%% map(Cat, Fun) -> [...] | {error, ...}
%%%    Cat = Uri
%%%    Fun = Fun(Key, Item) -> Val
%%%
%%% maps function over bucket
map({kvs, undefined, _} = Cat, Fun) when is_function(Fun) ->
   % check local passive category
   case ek:whereis(Cat) of
      undefined -> 
         {error, no_category};
      Pid       ->
         gen_kvs:map(type(Cat), Pid, Cat, Fun)
   end;

map({_, _, _} = Cat, Fun) when is_function(Fun) ->
   % check remote category
   gen_kvs:map(remote, undefined, Cat, Fun);
   
map(Cat, Fun) when Cat =/= undefined, 
                   is_function(Fun) ->
   kvs:map(ek_uri:new(Cat), Fun).   

   
%%%
%%% fold(Cat, Acc, Fun) -> [...] | {error, ...}
%%%    Cat  = Uri
%%%    Fun  = Fun(Key, Item, Acc) -> Val | undefined
%%%
%%% fold function over bucket
fold({kvs, undefined, _} = Cat, Acc, Fun) when is_function(Fun) ->
   % check local passive category
   case ek:whereis(Cat) of
      undefined -> 
         {error, no_category};
      Pid       ->
         gen_kvs:fold(type(Cat), Pid, Cat, Acc, Fun)
   end;

fold({_, _, _} = Cat, Acc, Fun) when is_function(Fun) ->
   % remote category
   gen_kvs:fold(remote, undefined, Cat, Acc, Fun);
   
fold(Cat, Acc, Fun) when Cat =/= undefined, 
                   is_function(Fun) ->
   kvs:fold(ek_uri:new(Cat), Acc, Fun).   

   
%%%------------------------------------------------------------------
%%%
%%% Private Functions
%%%
%%%------------------------------------------------------------------

%% detect type of category
type({kvs, undefined, Path}) ->
   case ek:whereis({kvs, undefined, <<Path/binary, "#key">>}) of
      undefined -> passive;
      _         -> active
   end.





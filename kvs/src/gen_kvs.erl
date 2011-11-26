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
-module(gen_kvs).
-behaviour(gen_server).

-author(dmitry.kolesnikov@nokia.com).
-include_lib("stdlib/include/qlc.hrl").

%%
%% TODO: refactor plugin interface

-export([
   start_link/1,
   % api
   new/1,
   put/5,
   has/4,
   get/4,
   remove/4,
   map/4,
   fold/5,
   % gen_server
   init/1, 
   handle_call/3,
   handle_cast/2, 
   handle_info/2, 
   terminate/2, 
   code_change/3,   
   % value process     
   val_init/2,
   val_terminate/2,
   get_val_prop/3,

   % plugin utility api
   %init/1,
   %init/2,
   %terminate/1,
   %key_to_pid/2,
   
   %key_map/3,
   %key_fold/4,
   % custom behaviour
   behaviour_info/1
]).

%%
%% debug macro
-ifdef(DEBUG).
-define(DEBUG(M), error_logger:info_report([{?MODULE, self()}] ++ M)).
-else.
-define(DEBUG(M), true).
-endif.


%%%------------------------------------------------------------------
%%%
%%% Behaviour interface
%%%
%%%------------------------------------------------------------------
behaviour_info(callbacks) ->
   [
      % plugin api 
      {put,  3},      %% put(Pid, Key, Item) -> ok | {error, ...}
      {has,  2},      %% has(Pid, Key)  -> true | false                     
      {get,  2},      %% get(Pid, Key)  -> {ok, Item} | {error, ...}
      {remove, 2},    %% remove(Pid, Key) -> ok | {error, ...}
      {map,    2},    %% map(Pid, Fun) -> Result | {error, ...}
      {fold,   3}     %% fold(Pid, Acc, Fun) -> Result | {error, ...}
      % server callback
   ];

behaviour_info(_) -> 
   undefined.
   
%%
-record(srv, {
   spec,   % category spec
   mod,    % module
   cat     % category state
}).   
   
%%%------------------------------------------------------------------
%%%
%%% gen_server
%%%
%%%------------------------------------------------------------------

%%
%% Start new category
start_link(Spec) ->
   gen_server:start_link(?MODULE, [Spec], []).
   
init([Spec]) ->
   Check = fun(X) ->
     lists:member(gen_kvs,
        proplists:get_value(behaviour, 
           X:module_info(attributes)
        )
     )
   end,
   Mod = case Check(proplists:get_value(storage, Spec)) of
      true  -> proplists:get_value(storage, Spec);
      false -> kvs_act
   end, 
   ?DEBUG([Spec]),
   {ok,
      #srv{
         spec = Spec,
         mod  = Mod,
         cat  = undefined
      }
   }.

%%
%% second phase constructor
handle_call(kvs_new, _, #srv{spec = Spec, mod = Mod} = S) ->
   case (catch Mod:new(Spec)) of
      {ok, Cat} ->
         Uri = proplists:get_value(uri, Spec),
         case proplists:is_defined(direct, Spec) of
            true  -> ek:register(Uri, {Mod, Cat});
            false -> ek:register(Uri)
         end,
         {reply, ok, S#srv{cat = Cat}};
      _  ->
         {stop, normal, S}
   end;
%%
%% Synchronous interface   
handle_call({kvs_put, Key, Val}, _, S) ->
   {reply, srv_put(Key, Val, S), S};   
handle_call({kvs_has, Key}, _, S) ->
   {reply, srv_has(Key, S), S};
handle_call({kvs_get, Key}, _, S) ->
   {reply, srv_get(Key, S), S};
handle_call({kvs_remove, Key}, _, S) ->
   {reply, srv_remove(Key, S), S};
handle_call({kvs_map, Fun}, _, S) ->
   {reply, srv_map(Fun, S), S};
handle_call({kvs_fold, Acc, Fun}, _, S) ->
   {reply, srv_fold(Acc, Fun, S), S};   
handle_call(_Req, _From, S) ->
   {reply, undefined, S}.

%%
%% Asynchronous interface
handle_cast({kvs_put, Key, Val}, S) ->
   srv_put(Key, Val, S),
   {noreply, S};
handle_cast({kvs_remove, Key}, S) ->  
   srv_remove(Key, S),
   {noreply, S};
handle_cast(_Req, State) ->
   {noreply, State}.   
   
%%
%% Asynchronous protocol
handle_info({kvs_put, Key, Val}, S) ->
   srv_put(Key, Val, S),
   {noreply, S};
handle_info({kvs_remove, Key}, S) ->  
   srv_remove(Key, S),
   {noreply, S};
handle_info(Msg, #srv{mod = Mod, cat=Cat} = S) ->
   {noreply, S#srv{cat = Mod:handle(Msg, Cat)}}.   
   
terminate(_Reason, S) ->
   Uri = proplists:get_value(uri, S#srv.spec),   
   ek:unregister(Uri),
   ok.
   
code_change(_OldVsn, State, _Extra) ->
   {ok, State}.    



%%%------------------------------------------------------------------
%%%
%%% api
%%%
%%%------------------------------------------------------------------

%%
%%
new(Pid) when is_pid(Pid) ->
   gen_server:call(Pid, kvs_new).

   
   
%%
%% put value into local passive category
put(passive, Pid, _Cat, Key, Val) when is_pid(Pid) -> 
   gen_server:call(Pid, {kvs_put, Key, Val});   

%%
%% put value into local passive category (direct call)
put(passive, {Mod, Ref}, _Cat, Key, Val) ->
   Mod:put(Key, Val, Ref);
   
%%
%% put value into local active category
put(active, _Pid, Cat, Key, Val) ->
   NPid = case key_to_pid(Cat, Key) of
      {error, not_found} -> ek:whereis(Cat);
      {ok, Pid}          -> Pid
   end,
   % either perform update at owner process or trigger element process creation 
   gen_server:call(NPid, {kvs_put, Key, Val});
   
%%
%% put value into remote category
put(remote, _Pid, {_,_,_}=Cat, Key, Val) ->
   ek:send(Cat, {kvs_put, Key, Val}).

   
   
   
%%
%% check key from local passive category
has(passive, Pid, _Cat, Key) when is_pid(Pid) ->
   gen_server:call(Pid, {kvs_has, Key});
  
%%
%% check key form local passive category (direct call)
has(passive, {Mod, Ref}, _Cat, Key) ->
   Mod:has(Key, Ref);   
   
%%
%% check key from local active category
has(active, _Pid, Cat, Key) ->   
   case key_to_pid(Cat, Key) of
      {error, not_found} -> false;
      {ok, _}            -> true
   end;
   
%%
%% check key from remote category
has(remote, _Pid, {_,_,_}=Cat, Key) ->
   throw(not_supported).
   


   
%%
%% get key from local passive category
get(passive, Pid, _Cat, Key) when is_pid(Pid) ->
   gen_server:call(Pid, {kvs_get, Key});

%%
%% get key form local passive category (direct call)
get(passive, {Mod, Ref}, _Cat, Key) ->
   Mod:get(Key, Ref);   
   
%%
%% get key from local active
get(active, _Pid, Cat, Key) ->   
   case key_to_pid(Cat, Key) of
      {ok, Pid}   -> gen_server:call(Pid, {kvs_get, Key});
      Err         -> Err
   end;

%%
%% get key from remote category
get(remote, _Pid, {_,_,_}=Cat, Key) ->
   throw(not_supported).
   
   
   
%%
%% remove key from local passive category
remove(passive, Pid, _Cat, Key) when is_pid(Pid) ->
   gen_server:call(Pid, {kvs_remove, Key});

%%
%% check key form local passive category (direct call)
remove(passive, {Mod, Ref}, _Cat, Key) ->
   Mod:remove(Key, Ref);   
   
%%
%% remove key from local active category
remove(active, _Pid, Cat, Key) ->
   case key_to_pid(Cat, Key) of
      {ok, Pid} -> gen_server:cast(Pid, {kvs_remove, Key});
      Err       -> Err
   end;

%%
%% remove key from remote category
remove(remote, _Pid, {_,_,_}=Cat, Key) ->
   ek:send(Cat, {kvs_remove, Key}).   
   
   
   
   
%%
%% map keys from local passive category
map(passive, Pid, _Cat, Fun) when is_pid(Pid) ->
   gen_server:call(Pid, {kvs_map, Fun});
   
%%
%% map keys form local passive category (direct call)
map(passive, {Mod, Ref}, _Cat, Fun) ->
   Mod:map(Fun, Ref);
   
%%
%% map keys from local active category
map(active, _Pid, {_,_,Path}=Cat, Fun) ->
   kvs:map(
      {kvs, undefined, <<Path/binary, "#key">>}, 
      fun(Key, Pid) ->
         {ok, Val} = gen_server:call(Pid, {kvs_get, Key}),
         Fun(Key, Val)
      end
   );   

%%
%% map keys from remote category
map(remote, _Pid, {_,_,_}=Cat, Fun) ->
   throw(not_supported).



%%
%% fold keys from local passive category
fold(passive, Pid, _Cat, Acc, Fun) when is_pid(Pid) ->
   gen_server:call(Pid, {kvs_fold, Acc, Fun});

%%
%% check key form local passive category (direct call)
fold(passive, {Mod, Ref}, _Cat, Acc, Fun) ->
   Mod:fold(Acc, Fun, Ref);
   
%%
%% fold keys from local active category
fold(active, _Pid, {_,_,Path}=Cat, Acc, Fun) ->
   kvs:fold(
      {kvs, undefined, <<Path/binary, "#key">>},
      Acc,
      fun(Key, Pid, Acc1) ->
         {ok, Val} = gen_server:call(Pid, {kvs_get, Key}),
         Fun(Key, Val, Acc1)
      end
   );   

%%
%% fold keys from remote category
fold(remote, _Pid, {_,_,_} = Cat, Acc, Fun) ->
   throw(not_supported).
   
   
   
%%%------------------------------------------------------------------
%%%
%%% Utility interface
%%%
%%%------------------------------------------------------------------

%%
%% register value key
val_init({kvs, undefined, Path}, Key) ->
   Cat = {kvs, undefined, <<Path/binary, "#key">>},
   ok = kvs:put(Cat, Key, self()).
   
val_terminate({kvs, undefined, Path}, Key) ->
   Cat = {kvs, undefined, <<Path/binary, "#key">>},
   ok = kvs:remove(Cat, Key).

%%
%%
%%
get_val_prop(Cat, Attr, Val) ->
   undefined.
   %{ok, Spec} = kvs:get(kvs_sys_cat, Cat),
   %case proplists:get_value(getter, Spec) of
   %   undefined -> undefined;
   %   Get       -> Get(Attr, Val)
   %end.   








   
%%%------------------------------------------------------------------
%%%
%%% Protected Functions (kvs interface)
%%%
%%%------------------------------------------------------------------

% 
% handle_has(Bucket, Key) ->
   % Bid = proplists:get_value(name,    Bucket),
   % Mod = proplists:get_value(storage, Bucket),
   % case kvs_sys:get(kvs_sys_ref, {keyspace, Bid}) of
      % {error, not_found} ->
         % case kvs_sys:get(kvs_sys_ref, Bid) of
            % {error, not_found} -> false;
            % {ok, Pid}          -> Mod:has(Pid, Key)
         % end;
      % {ok, Keyspace}     ->
         % % (keyspace partitioned) entity storage is resolved via bucket specific keyspace
         % case kvs_sys:get(Keyspace, Key) of
            % {error, not_found} -> false;
            % {ok, Pid}          -> Mod:has(Pid, Key)
         % end
   % end.
   % 
% handle_get(Bucket, Key) ->
   % Bid = proplists:get_value(name,    Bucket),
   % Mod = proplists:get_value(storage, Bucket),
   % case resolve(Bid, Key) of
      % undefined -> {error, not_found};
      % Pid       -> Mod:get(Pid, Key)
   % end.
% 
% handle_remove(Bucket, Key) ->
   % Bid = proplists:get_value(name,    Bucket),
   % Mod = proplists:get_value(storage, Bucket),
   % case resolve(Bid, Key) of
      % undefined -> ok;
      % Pid       -> Mod:remove(Pid, Key)
   % end,
   % notify(remove, Bucket, Key, undefined),
   % ok.
   % 
% handle_map(Bucket, Fun) ->
   % case lists:member(map, proplists:get_value(feature, Bucket, [])) of
      % false ->
         % {error, not_supported};
      % true  ->
         % Bid = proplists:get_value(name,    Bucket),
         % Mod = proplists:get_value(storage, Bucket),
         % % resolve keyspace management
         % case kvs_sys:get(kvs_sys_ref, {keyspace, Bid}) of
            % {error, not_found}  ->
               % % no key space management
               % {ok, Pid} = kvs_sys:get(kvs_sys_ref, Bid),
               % Mod:map(Pid, Fun);
            % {ok, Keyspace}      ->
               % % there is a key space management process
               % kvs_sys:map(Keyspace, 
                  % fun(Key, Pid) ->
                     % {ok, Item} = Mod:get(Pid, Key),
                     % Fun(Key, Item)
                  % end
               % )
         % end
   % end.
   % 
% handle_fold(Bucket, Acc, Fun) ->
   % case lists:member(map, proplists:get_value(feature, Bucket, [])) of
      % false ->
         % {error, not_supported};
      % true  ->
         % Bid = proplists:get_value(name,    Bucket),
         % Mod = proplists:get_value(storage, Bucket),
         % % resolve keyspace management
         % case kvs_sys:get(kvs_sys_ref, {keyspace, Bid}) of
            % {error, not_found}  ->
               % % no key space management
               % {ok, Pid} = kvs_sys:get(kvs_sys_ref, Bid),
               % Mod:fold(Pid, Acc, Fun);
            % {ok, Keyspace}      ->
               % % there is a key space management process
               % kvs_sys:fold(Keyspace, Acc,
                  % fun(Key, Pid, AccIn) ->
                     % {ok, Item} = Mod:get(Pid, Key),
                     % case Fun(Key, Item) of
                        % undefined -> AccIn;
                        % AccOut    -> AccOut
                     % end
                  % end
               % )
         % end
   % end.
   % 
 

%%%------------------------------------------------------------------
%%%
%%% Private Functions
%%%
%%%------------------------------------------------------------------
   
%%
%%
srv_put(Key, Val, #srv{spec = Spec, mod = Mod, cat = Cat}) ->
   case (catch Mod:put(Key, Val, Cat)) of
      ok  -> 
         case proplists:is_defined(event, Spec) of
            true  -> kvs_evt:notify(put, proplists:get_value(uri, Spec), Key, Val), ok;
            false -> ok
         end;
      Err ->
         Err
   end.

%%   
%%   
srv_has(Key, #srv{spec = Spec, mod = Mod, cat = Cat}) ->
   catch Mod:has(Key, Cat).

%%   
%%   
srv_get(Key, #srv{spec = Spec, mod = Mod, cat = Cat}) ->
   catch Mod:get(Key, Cat).

%%   
%%   
srv_remove(Key, #srv{spec = Spec, mod = Mod, cat = Cat}) ->
   case (catch Mod:remove(Key, Cat)) of
      ok ->
         case proplists:is_defined(event, Spec) of
            true  -> kvs_evt:notify(remove, proplists:get_value(uri, Spec), Key, undefined), ok;
            false -> ok
         end;
      Err ->
         Err
   end.

%%
%%
srv_map(Fun, #srv{spec = Spec, mod = Mod, cat = Cat}) ->
   catch Mod:map(Fun, Cat).
   
%%
%%
srv_fold(Acc, Fun, #srv{spec = Spec, mod = Mod, cat = Cat}) ->
   catch Mod:fold(Acc, Fun, Cat).   
   
   
%%   
%% key_to_pid(Cat, Key) -> {ok, Pid} | {error, ...} 
%%
%% lookup local process that holds a key
key_to_pid({kvs, undefined, Path}, Key) ->
   Cat = {kvs, undefined, <<Path/binary, "#key">>},
   case kvs:get(Cat, Key) of
      {ok, Pid} ->
         case is_process_alive(Pid) of
            true  -> 
               {ok, Pid};
            false -> 
               kvs:remove(Cat, Key),
               {error, not_found}
         end;
      Err       -> 
         Err
   end.
   
   
   
   

%%   
%%   
bin_to_hex(Bin) ->
   bin_to_hex(Bin, "").
   
bin_to_hex(<<>>, Acc) ->
   Acc;
bin_to_hex(<<X:8, T/binary>>, Acc) ->  
   bin_to_hex(T, Acc ++ [to_hex(X div 16), to_hex(X rem 16)]).
   
to_hex(X) when X < 10 ->
   $0 + X;
to_hex(X) ->
   $a + (X - 10).


% % resolve Pid of handler responsible for given key   
% resolve(Bucket, Key) ->
   % case kvs_sys:get(kvs_sys_ref, {keyspace, Bucket}) of
      % {error, not_found}   ->
         % case kvs_sys:get(kvs_sys_ref, Bucket) of
            % {error, not_found} -> 
               % error_logger:error_msg('KVS: access to undefined bucket ~p~n', [Bucket]),
               % undefined;
            % {ok, Pid}          -> 
               % Pid
         % end;
      % {ok, Keyspace}      ->
         % % (keyspace partitioned) entity storage is resolved via bucket specific keyspace
         % case kvs_sys:get(Keyspace, Key) of
            % {error, not_found} -> undefined;
            % {ok,    Pid}       -> Pid
         % end
   % end.
   % 
% %%%
% %%%  Notify
% %%%
% notify(put, Bucket, Key, Item) ->
   % case proplists:is_defined(event, Bucket) of
      % true  -> 
         % %Bname = proplists:get_value(name, Bucket),
         % %kvs_evt:put(Bname, Key, Item);
         % kvs_evt:put(Bucket, Key, Item);
      % false -> 
         % ok
   % end;
% 
% notify(remove, Bucket, Key, Item) ->
   % case proplists:is_defined(event, Bucket) of
      % true  -> 
         % %Bname = proplists:get_value(name, Bucket),
         % %kvs_evt:remove(Bname, Key, Item);
         % kvs_evt:remove(Bucket, Key, Item);
      % false -> 
         % ok
   % end;
   % 
% notify(_, _, _, _) -> 
   % ok.      
   % 

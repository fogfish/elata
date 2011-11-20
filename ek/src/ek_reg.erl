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
-module(ek_reg).
-author(dmitry.kolesnikov@nokia.com).

%%
%% Resource registry binds URI with resource id (e.g. Pid, Port, etc)
%% Implements Erlang BIF register/2, unregister/1, whereis/1 
%%

-export([
   start_link/0,
   register/2,
   unregister/1,
   whereis/1,
   registered/0,
   registered/1,
   remote/0,
   remote/1,
   q/3
   
   %registered/0,
   %registered/1
]).


%%
%%
start_link() ->
   %% isolate table ownership
   Pid = spawn_link(
      fun() ->
         ets:new(?MODULE, [public, named_table, ordered_set]),
         Loop = fun(X) ->
            receive
               _ -> X(X)
            end
         end,
         Loop(Loop)
      end
   ),
   {ok, Pid}.


%%
%% register(URI, Pid) -> ok
%%   Uri = list() | tuple()
%%   Pid = pid()
%%
%% Associates the name URI with a pid. Updates an assotiation if Uri is 
%% assotiated with invalid Pid
%% Failure: badarg if Pid is not an existing, if Uri is already in use,
%% or if Uri is not complient http://tools.ietf.org/html/rfc3986.
%%
register({_Scheme, _Node, _Path} = Uri, Pid) ->
   assert_pid(Pid),
   case ek_reg:whereis(Uri) of
      undefined -> ets:insert(?MODULE, {Uri, Pid}), ok;
      _         -> throw(badarg)
   end;
register(Uri, Pid) ->
   ek_reg:register(ek_uri:new(Uri), Pid).

%%
%% unredister(Uri) -> ok
%%   Uri = list() | tuple()
%%
%% Removes the registered name Uri, associated with a pid.
%% Failure: badarg if Uri is not a registered name, Uri is assotiated 
%% with invalid pid or if Uri is not complient http://tools.ietf.org/html/rfc3986.
unregister({_Scheme, _Node, _Path} = Uri) ->
   case ek_reg:whereis(Uri) of
      undefined -> throw(badarg);
      _         -> ets:delete(?MODULE, Uri), ok
   end;
unregister(Uri) ->   
   ek_reg:unregister(ek_uri:new(Uri)).
   
%%
%% whereis(Uri) -> pid() | undefined
%%    Uri = list() | tuple()
%%
%% Returns the pid or port identifier with the registered name Uri. 
%% Returns undefined if the name is not registered. 
whereis({_Scheme, _Node, _Path} = Uri) ->
   case ets:lookup(?MODULE, Uri) of
      [{Uri, Pid}] ->
         case is_process_alive(Pid) of
            true  -> Pid;
            false -> undefined
         end;
      _            -> 
         undefined
   end;
whereis(Uri) ->
   ek_reg:whereis(ek_uri:new(Uri)).
   
%%
%% q(Schema, Authority, Path) -> [Uri]
%%   Uri = {Schema, Authority, Path}
%%
%% query identities
q(Scheme, Authority, Path) ->
   lists:foldl(
      fun({{S,A,P}, Pid}, Acc) ->
         case is_process_alive(Pid) of
            true  -> 
               SB = Scheme(S),
               AB = Authority(A),
               PB = Path(P),
               if 
                  SB, AB, PB -> [{S,A,P} | Acc];
                  true       -> Acc
               end;
            false -> 
               ets:delete(?MODULE, {S,A,P}), Acc
         end
      end,
      [],
      ets:match_object(?MODULE, '_')
   ).
   
%%
%% registered() -> [Uri]
%%
%% Localy registered identifiers
registered() ->
   ek_reg:q(
      fun(_) -> true end,
      fun(undefined) -> true; (_) -> false end,
      fun(_) -> true end
   ).
   
%%
%% registered(Scheme) -> [Uri]
%%
%% Localy registered identifiers with schema restriction
registered(Schema) ->
   ek_reg:q(
      fun(X) -> if X =:= Schema -> true; true -> false end end,
      fun(undefined) -> true; (_) -> false end,
      fun(_) -> true end
   ). 
   
%%
%% remote() -> [Uri]
%%
%% Remotely registered identifiers
remote() ->
   ek_reg:q(
      fun(_) -> true end,
      fun(undefined) -> false; (_) -> true end,
      fun(_) -> true end
   ).
   
%%
%% remote(Scheme) -> [Uri]
%%
%% Remotely registered identifiers with schema restriction
remote(Schema) ->
   ek_reg:q(
      fun(X) -> if X =:= Schema -> true; true -> false end end,
      fun(undefined) -> false; (_) -> true end,
      fun(_) -> true end
   ).    
   
%%%------------------------------------------------------------------
%%%
%%% Private
%%%
%%%------------------------------------------------------------------

%%
%%
assert_pid(Pid) ->
   case is_process_alive(Pid) of
      false -> throw(badarg);
      true  -> true
   end.



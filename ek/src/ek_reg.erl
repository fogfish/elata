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
%% Process registry, 
%% incontrast to erlang BIF register/2, unregister/1, whereis/1 where only atom is used
%% The registry allows uri and tuples to be used as process identity
%%

-export([
   start_link/0,
   register/2,
   unregister/1,
   whereis/1,
   registered/0,
   registered/1
]).


%%
%%
start_link() ->
   ets:new(?MODULE, [public, named_table]),
   ok.


%%
%% register(Uri, Pid) -> true
%%   Uri = list() | tuple()
%%   Pid = pid()
%%
%% Associates the name URI with a pid. Updates an assotiation if Uri is 
%% assotiated with invalid Pid
%% Failure: badarg if Pid is not an existing, if Uri is already in use,
%% or if Uri is not complient http://tools.ietf.org/html/rfc3986.
%%
register(Uid, Pid) when is_tuple(Uid) ->
   assert_pid(Pid),
   case ek_reg:whereis(Uid) of
      undefined -> ets:insert(?MODULE, {Uid, Pid});
      _         -> throw(badarg)
   end;
register(Uri, Pid) ->
   ek_reg:register({uri, assert_uri(Uri)}, Pid).

%%
%% unredister(Uri) -> true
%%   Uri = list() | tuple()
%%
%% Removes the registered name Uri, associated with a pid.
%% Failure: badarg if Uri is not a registered name, Uri is assotiated 
%% with invalid pid or if Uri is not complient http://tools.ietf.org/html/rfc3986.
unregister(Uid) when is_tuple(Uid) ->
   case ek_reg:whereis(Uid) of
      undefined -> throw(badarg);
      _         -> ets:delete(?MODULE, Uid)
   end;
unregister(Uri) ->   
   ek_reg:unregister({uri, assert_uri(Uri)}).
   
%%
%% whereis(Uri) -> pid() | undefined
%%    Uri = list() | tuple()
%%
%% Returns the pid or port identifier with the registered name Uri. 
%% Returns undefined if the name is not registered. 
whereis(Uid) when is_tuple(Uid) ->
   case ets:lookup(?MODULE, Uid) of
      [{Uid, Pid}] ->
         case is_process_alive(Pid) of
            true  -> Pid;
            false -> undefined
         end;
      _            -> 
         undefined
   end;
whereis(Uri) ->
   ek_reg:whereis({uri, assert_uri(Uri)}).
   
   
%%
%% registered() -> [] 
%%
%% Returns a list of names which have been registered using register/2.
registered() ->
   lists:foldl(
      fun
         ({{uri, Uri}, Pid}, Acc) ->
            case is_process_alive(Pid) of
               true  -> [Uri | Acc];
               false -> ets:delete(?MODULE, {uri, Uri}), Acc
            end;
         ({Uid, Pid}, Acc) ->
            case is_process_alive(Pid) of
               true  -> [Uid | Acc];
               false -> ets:delete(?MODULE, Uid), Acc
            end
      end,
      [],
      ets:match_object(?MODULE, '_')
   ).

%%
%% registered(Grp) -> []
%%
%% Return a list of names which have been regsitered  using register/2
%% with tuple
registered(Grp) ->
    lists:foldl(
       fun({{_, Name}, Pid}, Acc) ->
          case is_process_alive(Pid) of
             true  -> 
                [{Grp, Name} | Acc];
             false ->
                ets:delete(?MODULE, {Grp, Name}), Acc
          end
       end,
       [],
       ets:match_object(?MODULE, {{Grp, '_'}, '_'})
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

%%
%%
assert_uri(Uri) ->
   U = ek_uri:new(Uri),
   case proplists:is_defined(schema, U) of
      false ->
         % then path is mandatory
         case proplists:is_defined(path, U) of
            false -> throw(badarg);
            true  -> 
              [N | _] = ek:node(),
              N ++ Uri
         end;
      true ->
         % then host & port are mandatory
         case proplists:is_defined(host, U) of
            false -> throw(badarg);
            true  ->
               case proplists:is_defined(port, U) of
                  false -> throw(badarg);
                  true  -> Uri
               end
         end
   end.
   


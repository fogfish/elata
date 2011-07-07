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
-module(gen_rest).
-author(dmitry.kolesnikov@nokia.com).

%%
%% Webmachine Resource Wrapper
%%

-export([
   dispatch/1,
   init/1,
   allowed_methods/2,
   content_types_provided/2,
   content_types_accepted/2,
   process_post/2,
   delete_resource/2,
   get_handle/2,
   post_handle/2,   
   put_handle/2,
   del_handle/2
]).

-include_lib("webmachine/include/webmachine.hrl").

-record(rest, {
   mod,
   func,
   flags
}).

%%
%% Converts dispatch table into webmachine format
dispatch(Mod) ->
   lists:map(
      fun({Flags, Uri, Func}) ->
         {List, _} = lists:foldr(
            fun
            ("$", {Lst,     _}) -> {Lst, true};
            ("*", {Lst, false}) -> {['*' | Lst], false};
            (Tkn, {Lst,  true}) -> {[Tkn, list_to_atom(Tkn) | Lst], false};
            (Tkn, {Lst, false}) -> {[Tkn | Lst], false}
            end,
            {[], false},
            string:tokens(Uri, "/")
         ),
        {
           List, 
           ?MODULE, 
           [#rest{
              mod=Mod,
              func=Func,
              flags=Flags
           }]
         }   
      end,
      Mod:resources()
   ).

init([State]) ->
   {ok, State}.
   
allowed_methods(R, #rest{flags=Flags} = State) ->
   Methods = lists:flatten(
      lists:map(
         fun
         ($*) -> ['GET', 'POST', 'PUT', 'DELETE'];
         ($c) -> 'PUT';
         ($r) -> 'GET';
         ($u) -> 'POST';
         ($d) -> 'DELETE'
         end,
         Flags
      )
   ),
   {Methods, R, State}.
   
   
content_types_provided(R, #rest{mod=Mod} = State) ->
   CT = lists:map(
      fun({X, _}) ->
         {X, get_handle}
      end,
      Mod:content_types()
   ),
   {CT, R, State}.

content_types_accepted(R, #rest{mod=Mod} = State) ->
   CT = lists:map(
      fun({X, _}) ->
         {X, put_handle}
      end,
      Mod:content_types()
   ),
   {CT, R, State}.
   
process_post(R, State) ->
   post_handle(R, State).
   
delete_resource(R, State) ->
   del_handle(R, State).
   
%% read
get_handle(R, #rest{mod=Mod, func=F} = State) ->
   case call(F, {read, wrq:disp_path(R), wrq:path_info(R)}) of
      {ok, Val} -> 
         Val1 = encode(
            string:tokens(wrq:get_req_header('Accept', R), ","),
            Mod:content_types(),
            Val
         ), 
         {Val1, R, State};                  
      % TODO: better MIME management
      {ok, Val, Mime} ->
         R1 = wrq:set_resp_header("content-type", Mime, R),
         {Val, R1, State};
      Err ->   
         {error_to_http(Err), R, State}
   end.

%% create
put_handle(R, #rest{mod=Mod, func=F} = State) ->
   %%io:format('~p~n',[wrq:req_body(R)]),
   Msg = decode(
      wrq:get_req_header('content-type', R), 
      Mod:content_types(),
      wrq:req_body(R)
   ),
   case call(F, {create, wrq:disp_path(R), wrq:path_info(R), Msg}) of
      ok ->
         {true, R, State};
      {ok, Val} ->
         Val1 = encode(
            string:tokens(wrq:get_req_header('Accept', R), ","), 
            Mod:content_types(), 
            Val
         ),
         R1 = wrq:set_resp_body(Val1, R),
         {true, R1, State};
      Err ->   
         {error_to_http(Err), R, State}
   end.
  
% update   
post_handle(R, #rest{mod=Mod, func=F} = State) ->
   Msg = decode(
      wrq:get_req_header('content-type', R), 
      Mod:content_types(),
      wrq:req_body(R)
   ),
   case call(F, {update, wrq:disp_path(R), wrq:path_info(R), Msg}) of
      ok ->
         {true, R, State};
      {ok, Val} ->
         Val1 = encode(
            string:tokens(wrq:get_req_header('Accept', R), ","), 
            Mod:content_types(), 
            Val
         ),
         R1 = wrq:set_resp_body(Val1, R),
         {true, R1, State};
      Err ->   
         {error_to_http(Err), R, State}
   end.

% delete   
del_handle(R, #rest{mod=Mod, func=F} = State) ->
   case call(F, [{delete, wrq:disp_path(R), wrq:path_info(R)}]) of
      ok ->
         {true, R, State};
      {ok, Val} ->
         Val1 = encode(
            string:tokens(wrq:get_req_header('Accept', R), ","), 
            Mod:content_types(), 
            Val
         ),
         R1 = wrq:set_resp_body(Val1, R),
         {true, R1, State};
      Err ->   
         {error_to_http(Err), R, State}
   end.      

%%%------------------------------------------------------------------
%%%
%%% Private Functions
%%%
%%%------------------------------------------------------------------
encode([Enc | H], Ctypes, Value) ->
   case proplists:get_value(Enc, Ctypes, undefined) of
      undefined ->         
         encode(H, Ctypes, Value);
      Fun  ->
         apply(Fun, [{encode, Value}])
   end;
   
encode([], Ctypes, Value) ->
   %% first cntent type is assumed to be a default
   {_, Fun} = lists:nth(1, Ctypes),
   apply(Fun, [{encode, Value}]).
   

decode(Enc, Ctypes, Value) ->
   [Enc1 | _ ] = string:tokens(Enc, ";"), %drop charset 
   case proplists:get_value(Enc1, Ctypes, undefined) of
      undefined ->     
         erlang:throw({error, media_type});
         %Fun = proplists:get_value(default, Ctypes),
         %apply(Fun, [{decode, Value}]);
      Fun  ->
         apply(Fun, [{decode, Value}])
   end.   


call(F, Req) when is_function(F) ->
   apply(F, [Req]);
call({M, F, A}, Req) ->
   apply(M, F, A ++ [Req]).
   
%%   
error_to_http({error, not_found}) -> 
   {halt, 404};
error_to_http(_) -> 
   {halt, 500}. 
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
-module(ek_prot).
-author(dmitry.kolesnikov@nokia.com).
-include("include/ek.hrl").

%%
%% Implemeny ek protocol
%%

-export([
   send/2,
   multicast/2,
   recv/1
]).


send(Uri, Data) ->
   U    = ek_uri:new(Uri),
   Node = atom_to_list(proplists:get_value(schema, U)) ++ "://" ++ 
          binary_to_list(proplists:get_value(host, U)) ++ ":" ++
          integer_to_list(proplists:get_value(port, U)),
   case ets:lookup(ek_nodes, Node) of
      [N] ->
         Msg = term_to_binary({msg, Uri, Data}),
         gen_fsm:send_event(N#ek_node.pid, {tx, Msg});
      _             ->
         {error, not_found}
   end.

multicast(Uris, Data) ->
   lists:foreach(fun(U) -> send(U, Data) end, Uris).
   

recv(Msg) ->
   {msg, Uri, Data} = binary_to_term(Msg),
   U = ek_uri:new(Uri),
   case ets:lookup(ek_dispatch, proplists:get_value(path, U)) of
      []    -> ok;
      List  ->
         [ Pid ! Data || {_, Pid} <- List],
         ok
   end.
   
%%%------------------------------------------------------------------
%%%
%%% Private
%%%
%%%------------------------------------------------------------------

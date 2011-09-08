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
-module(kvs_evt).
-author(dmitry.kolesnikov@nokia.com).

%%
%% ELATA: KVS I/O events
%%

%% Public API
-export([
   start_link/0,
   subscribe/1,
   unsubscribe/1,
   create/2,
   insert/2,
   lookup/2,
   delete/2
]).

start_link() ->
   gen_event:start_link({local, ?MODULE}).

subscribe({Handler, Args}) ->
   gen_event:add_sup_handler(?MODULE, Handler, Args);
subscribe(Handler) ->
   io:format('subscribe ~p~n', [Handler]),
   gen_event:add_sup_handler(?MODULE, Handler, []).
   
unsubscribe({Handler, Args}) ->
   gen_event:delete_handler(?MODULE, Handler, Args);
unsubscribe(Handler) ->
   gen_event:delete_handler(?MODULE, Handler, []).   
   
   
create(Ns, Entity) ->
   io:format('do notify ~p~n', [Entity]),
   gen_event:notify(?MODULE, {create, Ns, Entity}).
   
insert(Ns, Entity) ->
   gen_event:notify(?MODULE, {insert, Ns, Entity}).
   
lookup(Ns, Key) ->
   gen_event:notify(?MODULE, {lookup, Ns, Key}).

delete(Ns, Key) ->
   gen_event:notify(?MODULE, {delete, Ns, Key}).
   
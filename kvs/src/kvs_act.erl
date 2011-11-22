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
-module(kvs_act).
-behaviour(gen_kvs).
-author(dmitry.kolesnikov@nokia.com).

%%
%% Active process category
%%

-export([
   start_link/1,
   new/1,
   % gen_kvs
   put/3
]).

%%
%% debug macro
-ifdef(DEBUG).
-define(DEBUG(M), error_logger:info_report([{?MODULE, self()}] ++ M)).
-else.
-define(DEBUG(M), true).
-endif.

%%
%% Active key/val category
%%    Storage specific options
%%       {type,   ...} type, (default set)
%%       {scope,  ...} scope (default private)
start_link(Spec) ->
   gen_kvs:start_link(Spec).

new(Spec) ->
   % create a keyspace category
   Uri = proplists:get_value(uri, Spec),
   {act, undefined, Path} = ek_uri:new(Uri),
   {ok, _} = kvs:new({kvs, undefined, <<Path/binary, "#key">>}, [{storage, kvs_ets}, direct]),
   % register an element factory
   kvs_sup:start_factory(Spec).
   

%%%------------------------------------------------------------------   
%%%
%%% gen_kvs
%%%
%%%------------------------------------------------------------------

%%
%%
put(Key, Val, Ref) ->
   {ok, _} = supervisor:start_child(Ref, [Key, Val]),
   ok.


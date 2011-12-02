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
-module(kvs_fs).
-behaviour(gen_kvs).
-author(dmitry.kolesnikov@nokia.com).
-include_lib("stdlib/include/qlc.hrl").

%%
%%  Flat file system category 
%%

-export([
   start_link/1,
   new/1,
   % gen_kvs
   put/3,
   has/2,
   get/2,
   remove/2
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
%%       {root, ...}
%%       {type, binary | file}
start_link(Spec) ->
  gen_kvs:start_link(Spec).
  
new(Spec) ->
   Root = proplists:get_value(root, Spec),
   Type = proplists:get_value(type, Spec, binary),
   {ok, {Type, Root}}.

%%%------------------------------------------------------------------   
%%%
%%% gen_kvs
%%%
%%%------------------------------------------------------------------

%%
%%
put(Key, Val, {binary, Root}) ->
   write([Root, key_to_stream(Key)], Val);   

put(Key, Val, {file, Root}) ->
   write([Root, key_to_file(Key)], Val).


%%
%%
has(Key, {binary, Root}) ->
   filelib:is_file([Root, key_to_stream(Key)]);
   
has(Key, {file, Root}) ->
   filelib:is_file([Root, key_to_file(Key)]).
   
%%
%%
get(Key, {binary, Root}) ->
   read([Root, key_to_stream(Key)]);

get(Key, {file, Root}) ->
   read([Root, key_to_file(Key)]).
   
%%
%%
remove(Key, {binary, Root}) ->
   file:delete([Root, key_to_stream(Key)]);
   
remove(Key, {file, Root}) ->   
   file:delete([Root, key_to_file(Key)]).

%%%------------------------------------------------------------------   
%%%
%%% Private functions
%%%
%%%------------------------------------------------------------------

%%
%% converts a key into file name
key_to_stream(Key) ->
   Hash = crypto:sha(term_to_binary(Key)),
   Hex  = [ integer_to_list(X, 16) || X <- binary_to_list(Hash) ],
   File = lists:append(Hex),
   "/" ++ lists:sublist(File, 2) ++ "/" ++ File.
   
key_to_file(Key) when is_list(Key) ->
   "/" ++ Key;
key_to_file(Key) when is_binary(Key) ->
   "/" ++ binary_to_list(Key);
key_to_file(Key) when is_atom(Key) ->
   "/" ++ atom_to_list(Key).
   
%%
%%
write(File, Data) when is_binary(Data) orelse is_list(Data) ->
   ?DEBUG([{write, File}]),
   filelib:ensure_dir(File),
   file:write_file(File, Data);
   
write(File, Data) ->
   ?DEBUG([{write, File}]),
   filelib:ensure_dir(File),
   file:write_file(File, term_to_binary(Data)).
   
%%
%%
read(File) ->
   ?DEBUG([{read, File}]),
   case file:read_file(File) of
      {ok, <<131, S/binary>> = Data} -> 
         {ok, binary_to_term(Data)};
      {ok, Data} -> 
         {ok, Data};
      {error, enoent} ->
         {error, not_found};
      Err -> Err
   end.
   
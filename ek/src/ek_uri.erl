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
-module(ek_uri).
-author(dmitry.kolesnikov@nokia.com).

%%
%% URI is used as node/process identity within Erlang Cluster
%% 

-export([
   new/1,
   host/1,
   path/1
]).

%%
%% parses binary as URI tuples
new(Str) when is_list(Str) ->
   uri(list_to_binary(Str), schema, <<>>, []);
new(Str) ->
   uri(Str, schema, <<>>, []).
      
%% end of schema
uri(<<":", Str/binary>>, schema, Acc, U) ->
   % TODO: security threat
   uri(Str, null, <<>>, [{schema, binary_to_atom(Acc, utf8)} | U]);

%% end of host
uri(<<":", Str/binary>>, host, Acc, U) ->
   uri(Str, port, <<>>, [{host, Acc} | U]);
      
%% start of host:port
uri(<<"//", Str/binary>>, _Tag, _Acc, U) ->
   uri(Str, host, <<>>, U);

%% end of port or host, start path or schema is ommited
uri(<<"/", Str/binary>>, schema, Acc, U) ->
   uri(Str, path, <<"/">>, U);
uri(<<"/", Str/binary>>, host, Acc, U) ->
   uri(Str, path, <<"/">>, [{host, Acc} | U]);

uri(<<"/", Str/binary>>, port, Acc, U) ->
   uri(Str, path, <<"/">>, [{port, list_to_integer(binary_to_list(Acc))} | U]);
   
%% end of path, start query
uri(<<"?", Str/binary>>, path, Acc, U) ->
   uri(Str, q, <<>>, [{path, Acc} | U]);

uri(<<"?", Str/binary>>, fragment, Acc, U) ->
   uri(Str, q, <<>>, [{fragment,Acc} | U]);
   
%% end of query, start fragment
uri(<<"#", Str/binary>>, path, Acc, U) ->
   uri(Str, fragment, <<>>, [{path, Acc} | U]);
   
uri(<<"#", Str/binary>>, q, Acc, U) ->
   uri(Str, fragment, <<>>, [{q, Acc} | U]);   
   
%% accumulates token
uri(<<H:8, T/binary>>, Tag, Acc, Uri) ->
   uri(T, Tag, <<Acc/binary, H>>, Uri);

uri(<<>>, port, Acc, U) ->
   [{port, list_to_integer(binary_to_list(Acc))} | U];
uri(<<>>, Tag, Acc, U) ->
   [{Tag, Acc} | U].
    

%%
%% return schema://host:port
host(U) when is_list(U) ->
   case proplists:is_defined(schema, U) of
      true  ->   
         atom_to_list(proplists:get_value(schema, U)) ++ "://" ++ 
         binary_to_list(proplists:get_value(host, U)) ++ ":" ++
         integer_to_list(proplists:get_value(port, U));
      false ->
         N = new(U),
         atom_to_list(proplists:get_value(schema, N)) ++ "://" ++ 
         binary_to_list(proplists:get_value(host, N)) ++ ":" ++
         integer_to_list(proplists:get_value(port, N))
   end.

%% return /path
path(U) ->
   case proplists:is_defined(schema, U) of
      true  ->
         binary_to_list(proplists:get_value(path, U));
      false ->
         N = new(U),
         binary_to_list(proplists:get_value(path, N))
   end.
   
   
   
%to_binary(#uri{schema = Schema, host = Host, port = Port, 
%               path = Path, q = Query, fragment = Frag}) ->
%   Bschema = atom_to_binary(Schema, utf8),
%   Bport   = list_to_binary(integer_to_list(Port)),
%   Bfrag   = case erlang:byte_size(Frag) of
%      0 -> <<>>;
%      _ -> <<"#", Frag/binary>>
%   end,
%   Bq      = case erlang:byte_size(Query) of
%     0 -> <<>>;
%     _ -> <<"?", Query/binary>>
%   end,
%   <<Bschema/binary, "://", Host/binary, ":", Bport/binary, 
%     Path/binary, Bfrag/binary, Bq/binary>>.
   
   




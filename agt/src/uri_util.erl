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
-module(uri_util).
-author(dmitry.kolesnikov@nokia.com).
-include("include/def.hrl").

%%
%% ELATA: URI Parser
%%    Parses a URI as list of tuples.
%% 

-export([
   new/1,
   to_binary/1
]).

%%
%% parses binary as URI tuples
new(Str) ->
   uri(Str, schema, <<>>, #uri{}).
      
%% end of schema
uri(<<":", Str/binary>>, schema, Acc, U) ->
   % TODO: security threat
   uri(Str, null, <<>>, U#uri{schema = binary_to_atom(Acc, utf8)});

%% end of host
uri(<<":", Str/binary>>, host, Acc, U) ->
   uri(Str, port, <<>>, U#uri{host = Acc});
      
%% start of host:port
uri(<<"//", Str/binary>>, _Tag, _Acc, Uri) ->
   uri(Str, host, <<>>, Uri);

%% end of port or host, start path
uri(<<"/", Str/binary>>, host, Acc, U) ->
   uri(Str, path, <<"/">>, U#uri{host = Acc});

uri(<<"/", Str/binary>>, port, Acc, U) ->
   uri(Str, path, <<"/">>, U#uri{port = list_to_integer(binary_to_list(Acc))});
   
%% end of path, start query
uri(<<"?", Str/binary>>, path, Acc, U) ->
   uri(Str, 'query', <<>>, U#uri{path = Acc});

uri(<<"?", Str/binary>>, fragment, Acc, U) ->
   uri(Str, 'query', <<>>, U#uri{fragment = Acc});
   
%% end of query, start fragment
uri(<<"#", Str/binary>>, path, Acc, U) ->
   uri(Str, fragment, <<>>, U#uri{path = Acc});
   
uri(<<"#", Str/binary>>, 'query', Acc, U) ->
   uri(Str, fragment, <<>>, U#uri{q = Acc});   
   
%% accumulates token
uri(<<H:8, T/binary>>, Tag, Acc, Uri) ->
   uri(T, Tag, <<Acc/binary, H>>, Uri);

uri(<<>>, path, Acc, U) ->
   U#uri{path = Acc};
uri(<<>>, 'query', Acc, U) ->
   U#uri{q = Acc};
uri(<<>>, fragment, Acc, U) ->
   U#uri{fragment = Acc}.
    
   
to_binary(#uri{schema = Schema, host = Host, port = Port, 
               path = Path, q = Query, fragment = Frag}) ->
   Bschema = atom_to_binary(Schema, utf8),
   Bport   = list_to_binary(integer_to_list(Port)),
   Bfrag   = case erlang:byte_size(Frag) of
      0 -> <<>>;
      _ -> <<"#", Frag/binary>>
   end,
   Bq      = case erlang:byte_size(Query) of
     0 -> <<>>;
     _ -> <<"?", Query/binary>>
   end,
   <<Bschema/binary, "://", Host/binary, ":", Bport/binary, 
     Path/binary, Bfrag/binary, Bq/binary>>.
   
   
%%
%% EUnit
%%
-include_lib("eunit/include/eunit.hrl").

uri1_test() ->
   {uri,
      http,
      <<"localhost">>,
      80,
      <<"/path/to">>,
      <<"blah">>,
      <<"a=b">>
   }  = uri_util:new(<<"http://localhost:80/path/to#blah?a=b">>).
   
uri2_test() ->  
   {uri,
      http,
      <<"localhost">>,
      80,
      <<"/path/to">>,
      undefined,
      undefined
   }  = uri_util:new(<<"http://localhost:80/path/to">>).



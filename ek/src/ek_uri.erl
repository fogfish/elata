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
%% Implements http://tools.ietf.org/html/rfc3986
%%
%% The following are two example URIs and their component parts:
%%
%%         foo://example.com:8042/over/there?name=ferret#nose
%%         \_/   \______________/\_________/ \_________/ \__/
%%          |           |            |            |        |
%%       scheme     authority       path        query   fragment
%%          |   _____________________|__
%%         / \ /                        \
%%         urn:example:animal:ferret:nose
%%
%% URI is enforsed as an identity and represented via tuple:
%% {schema, <<"authority">>, <<"path">>}
%%    
%% TODO:
%%   - query
%%   - fragment

-export([
   new/1,
   schema/1,
   host/1,
   port/1,
   authority/1,
   q/1,
   fragment/1,
   path/1,
   to_binary/1,
   hash/1,
   hash/2,
   lhash/1,
   lhash/2
]).

-define(NIL, {undefined, undefined, undefined}).

%%
%% new(URI) -> {Schema, Authority, Path}
%%   URI       = list() | binary()
%%   Schema    = atom()
%%   Authority = binary()
%%   Path      = binary()
%%
%% parses URI into tuple, fails with badarg if invalid URI
%%
new({Schema, Authority, Path}) when is_atom(Schema), 
                                    is_binary(Path) ->
   {Schema, Authority, Path};                                 
new(Uri) when is_list(Uri) ->
   p_uri(list_to_binary(Uri), schema, <<>>, ?NIL);
new(Uri) when is_binary(Uri)->
   p_uri(Uri, schema, <<>>, ?NIL).
   
%%
%% schema(URI) -> atom()
%%   URI  = list() | binary() | tuple()
schema({undefined, _, _}) ->
   undefined;
schema({Schema, _, _}) ->
   Schema;
schema(Uri) ->
   schema(new(Uri)).

   
   
%%
%% host(URI) -> binary()
%%   URI  = list() | binary() | tuple()
host({_, undefined, _}) ->
   undefined;
host({_, Auth, _}) ->
   {_, Host, _} = p_auth(Auth, host, <<>>, ?NIL),
   Host;
host(Uri) ->
   host(new(Uri)).
   
%%
%% port(URI) -> integer()
%%   URI  = list() | binary() | tuple()
port({_, undefined, _}) ->
   undefined;
port({Schema, Auth, _}) ->
   case p_auth(Auth, host, <<>>, ?NIL) of
      {_, _, undefined} -> schema_to_port(Schema);
      {_, _, Port}      -> Port
   end;
port(Uri) ->
   port(new(Uri)).
 
   
%%
%% authority(URI) -> binary()
%%   URI  = list() | binary() | tuple()
authority({_, undefined, _}) ->
   undefined;
authority({_, Auth, _}) ->
   Auth;
authority(Uri) ->
   authority(new(Uri)).

%%
%% path(URI) -> binary()
%%   URI  = list() | binary() | tuple()
path({_, _, In}) ->
   {Path, _, _} = p_path(In, path, <<>>, ?NIL),
   Path;
path(Uri) ->
   path(new(Uri)).      

   
%%
%% query(URI) -> binary()
%%   URI   = list() | binary() | tuple()
q({_,_, Path}) ->
   {_, Query, _} = p_path(Path, path, <<>>, ?NIL),
   Query;
q(Uri) ->
   q(new(Uri)).

%%
%% fragment(URI) -> binary()
%%   URI   = list() | binary() | tuple()
fragment({_,_, Path}) ->
   {_, _, Frag} = p_path(Path, path, <<>>, ?NIL),
   Frag;
fragment(Uri) ->
   fragment(new(Uri)).   
   
   
%%
%% hash(Uri)            -> binary()
%% lhash(Uri)           -> list()
%% hash(component, Uri) -> binary()
%% lhash(component, Uri)-> list()
%%
hash(Uri) ->
   crypto:sha(to_binary(Uri)).   
   
hash(authority, {_,Auth,_}) ->
   Sfx = string:to_lower(
         integer_to_list(erlang:phash2(Auth), 16)
   ),
   Len = 8 - string:len(Sfx),
   hex_to_bin(string:chars($0, Len) ++ Sfx);
hash(path, {_,_,Path}) ->
   crypto:sha(to_binary(Path));
hash(Part, Uri) ->
   hash(Part, new(Uri)).

lhash(Uri) ->
   bin_to_hex(hash(Uri)).
lhash(Part, Uri) ->
   bin_to_hex(hash(Part, Uri)).
   
%%
%% to_binary(URI) -> binary()
%%    URI  = list() | binary() | tuple()
to_binary({Schema, undefined, Path}) ->
   S = atom_to_binary(Schema, utf8),
   <<S/binary, ":", Path/binary>>;
to_binary({Schema, Authority, Path}) ->
   S = atom_to_binary(Schema, utf8),
   <<S/binary, "://", Authority/binary, Path/binary>>;
to_binary(Uri) ->
   to_binary(new(Uri)).
   
   
%%%------------------------------------------------------------------
%%%
%%% Private
%%%
%%%------------------------------------------------------------------

%%      URI         = scheme ":" hier-part [ "?" query ] [ "#" fragment ]
%%
%%      hier-part   = "//" authority path-abempty
%%                  / path-absolute
%%                  / path-rootless
%%                  / path-empty
%%

%% end of schema
p_uri(<<":", Str/binary>>, schema, Acc, {S,A,P}) ->
   % TODO: binary_to_atom security OOM issue
   % Note: end-of-schema implies path
   p_uri(Str, path, <<>>, {binary_to_atom(Acc, utf8), A, P});

%% start of authority
p_uri(<<"//", Str/binary>>, path, _, U) ->
   p_uri(Str, auth, <<>>, U);   
   
%% start of path
p_uri(<<"/", Str/binary>>, auth, Acc, {S,A,P}) ->
   p_uri(Str, path, <<"/">>, {S, Acc, P});   
   
%% accumulates token
p_uri(<<H:8, T/binary>>, Tag, Acc, U) ->
   p_uri(T, Tag, <<Acc/binary, H>>, U);   
   
%% eof   
p_uri(<<>>, schema, <<>>, {S,A,_}) ->
   {S,A,<<"/">>};
p_uri(<<>>, schema, Acc, {S,A,P})  ->
   {S,A,Acc};
p_uri(<<>>, auth, Acc, {S,_,_}) ->
   % no path, set default
   {S, Acc, <<"/">>};
p_uri(<<>>, path, Acc, {S,A,P}) ->
   {S, A, Acc}.
   

%%
%% authority   = [ userinfo "@" ] host [ ":" port ]
%%

%% end of userinfo
p_auth(<<"@", Str/binary>>, host, Acc, {_, H, P}) ->
   p_auth(Str, host, <<>>, {Acc, H, P});
   
%% end of host
p_auth(<<":", Str/binary>>, host, Acc, {U, _, P}) ->
   p_auth(Str, port, <<>>, {U, Acc, P});
   
%% accumulates token
p_auth(<<H:8, T/binary>>, Tag, Acc, A) ->
   p_auth(T, Tag, <<Acc/binary, H>>, A);

%% eof
p_auth(<<>>, host, Acc, {U, _, P}) ->
   {U, Acc, P};
p_auth(<<>>, port, Acc, {U, H, _}) ->
   {U, H, list_to_integer(binary_to_list(Acc))}.
   
%%
%%   path: /over/there?name=ferret#nose
%%
p_path(<<"?", Str/binary>>, path, Acc, {_, Q, F}) ->
   p_path(Str, 'query', <<>>, {Acc, Q, F});
   
p_path(<<"#", Str/binary>>, path, Acc, {_, Q, F}) ->
   p_path(Str, fragment, <<>>, {Acc, Q, F});
   
p_path(<<"#", Str/binary>>, 'query', Acc, {P, _, F}) ->
   p_path(Str, fragment, <<>>, {P, Acc, F});
   
%% accumulates token
p_path(<<H:8, T/binary>>, Tag, Acc, P) ->
   p_path(T, Tag, <<Acc/binary, H>>, P);   
   
%% eof
p_path(<<>>, path, Acc, {_, Q, F}) ->
   {Acc, Q, F};
p_path(<<>>, 'query', Acc, {P, _, F}) ->
   {P, Acc, F};
p_path(<<>>, fragment, Acc, {P, Q, _}) ->
   {P, Q, Acc}.
   
   

%%
%% Maps schema to default ports
schema_to_port(http)  -> 80;
schema_to_port(https) -> 443;
schema_to_port(_)     -> undefined.
   

%%   
%% binary to hex
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

%%
%% hex to binary
hex_to_bin(Hex) ->
   hex_to_bin(Hex, <<>>).

hex_to_bin([], Acc) ->
   Acc;
hex_to_bin([H, L | T], Acc) ->
   V = to_int(H) * 16 + to_int(L),
   hex_to_bin(T, <<Acc/binary, V>>).

to_int(C) when C >= $a, C =< $f ->
   10 + (C - $a);
to_int(C) when C >= $0, C =< $9 ->
   C - $0.  
   

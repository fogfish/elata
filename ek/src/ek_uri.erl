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
%% URI is enforsed as an identity and represented by tuple.
%% nested tuples allows pattern matching against schema
%% {Schema, {UserInfo, Host, Port, Path, Query, Fragment}}
%%    
%% TODO:
%%   - IPv6 support

-export([
   new/0,
   new/1,
   get/2,
   set/3,
   append/3,
   insert/3,
   path/2,
   to_list/1,
   to_binary/1,
   to_path/1,
   hash/1
]).

%%
%% internal representation
-record(uri, {
   userinfo = [],
   host     = [],
   port     = undefined,
   path     = [],
   q        = [],
   fragment = []
}).

-define(NIL, {[], [], []}).

%%
%% new(URI) -> {uri, ...}
%%   URI       = list() | binary() | uri()
%%
%% parses URI into tuple, fails with badarg if invalid URI
%%
new() ->
   {undefined, #uri{}}.
new({_,U} = Uri) when is_record(U, uri) ->
   Uri;
new(Uri) when is_binary(Uri) ->
   new(binary_to_list(Uri));
new(Uri) when is_list(Uri) ->
   {Schema, Auth, Part} = p_uri(Uri, schema, "", ?NIL),
   {User,   Host, Port}  = p_auth(Auth, host, "", ?NIL),
   {Path,   Q,    Frag} = p_path(Part, path, "", ?NIL),
   {Schema,
      #uri{
      userinfo = User,
      host     = Host,
      port     = schema_to_port(Schema, Port),
      path     = Path,
      q        = Q,
      fragment = Frag
      }
   }.
   
%%
%% get(Item, Uri) -> list()
%%
get(schema, {S, _}) ->
   S;
get(authority, {_, Uri}) when is_record(Uri, uri) ->
   to_list(authority, Uri);
get(userinfo, {_, #uri{userinfo = U}}) ->
   U;
get(host, {_, #uri{host = H}}) ->
   H;
get(port, {_, #uri{port = P}}) ->
   P;
get(resource, {_, Uri}) when is_record(Uri, uri) ->
   to_list(resource, Uri);
get(path, {_, #uri{path = P}}) ->
   P;
get(q, {_, #uri{q = Q}}) ->
   Q;
get(fragment, {_, #uri{fragment = F}}) ->
   F;
get(Item, Uri) when is_list(Uri) ->   
   get(Item, new(Uri));
get(Item, Uri) when is_binary(Uri) ->   
   get(Item, new(Uri)).
   
%%
%% set(Item, Val, Uri) -> Uri
%%
set(schema, Value, {_, Uri}) when is_record(Uri, uri) ->
   {Value, Uri};
set(authority, Value, {Schema, Uri}) when is_record(Uri, uri) ->
   {User,   Host, Port}  = p_auth(Value, host, "", ?NIL),
   {Schema, Uri#uri{
      userinfo = User,
      host     = Host,
      port     = schema_to_port(Schema, Port)
   }};
set(userinfo, Value, {Schema, Uri}) when is_record(Uri, uri) ->
   {Schema, Uri#uri{userinfo = Value}};
set(host, Value, {Schema, Uri}) when is_record(Uri, uri) ->
   {Schema, Uri#uri{host = Value}};
set(port, Value, {Schema, Uri}) when is_record(Uri, uri), is_integer(Value) ->   
   {Schema, Uri#uri{port = Value}};
set(path, Value, {Schema, Uri}) when is_record(Uri, uri) ->
   {Schema, Uri#uri{path = Value}};
set(q, Value, {Schema, Uri}) when is_record(Uri, uri) ->
   {Schema, Uri#uri{q = Value}};
set(fragment, Value, {Schema, Uri}) when is_record(Uri, uri) ->
   {Schema, Uri#uri{fragment = Value}};
set(Item, Value, Uri) when is_list(Uri) ->   
   set(Item, Value, new(Uri));
set(Item, Value, Uri) when is_binary(Uri) ->   
   set(Item, Value, new(Uri)).   


%%
%% append(Item, Val, Uri) -> Uri
%% TODO: other components
append(path, Value, {Schema, #uri{path = Path} = Uri}) ->
   {Schema, Uri#uri{path = Path ++ Value}};
append(Item, Value, Uri) ->
   append(Item, Value, new(Uri)).   
  
%%
%% insert(Item, Val, Uri) -> Uri
%% TODO: other components
insert(path, Value, {Schema, #uri{path = Path} = Uri}) ->
   {Schema, Uri#uri{path = Value ++ Path}};
insert(Item, Value, Uri) ->
   insert(Item, Value, new(Uri)).   
   
   
   
   
%%
%% path(Nth, URI) -> list()
%%
path(Nth, {_, #uri{path = Path}}) when Nth >= 0 ->
   Tkns = string:tokens(Path, "/"),
   {_, List} = lists:split(Nth, Tkns),
   lists:foldl(
      fun
         ("", Acc) -> Acc;
         (X,  Acc) -> Acc ++ "/" ++ X
      end, 
      "",
      List
   );
path(Nth, {_, #uri{path = Path}}) when Nth < 0 ->
   Tkns = string:tokens(Path, "/"),
   {List, _} = lists:split(length(Tkns) + Nth, Tkns),
   lists:foldl(
      fun
         ("", Acc) -> Acc;
         (X,  Acc) -> Acc ++ "/" ++ X 
      end, 
      "",
      List
   );
path(Nth, Uri) ->
   path(Nth, new(Uri)).
   
 
%%
%%
to_list({Schema, #uri{userinfo = [], host = [], port = undefined} = Uri})  ->
   atom_to_list(Schema) ++ ":" ++ to_list(resource, Uri);
to_list({Schema, Uri})  ->
   atom_to_list(Schema) ++ "://" ++ to_list(authority, Uri) ++ to_list(resource, Uri);
to_list(Uri) ->
   to_list(new(Uri)).
   
%%
%% to_binary(URI) -> binary()
%%    URI  = list() | binary() | tuple()
to_binary(Uri) ->
   list_to_binary(to_list(Uri)).

%%
%% to_path(URI) -> binary()
%%
to_path({_,_} = Uri) ->
   Q = case get(q, Uri) of
          [] -> [];
          Rq -> integer_to_list(erlang:phash(Rq, 16#FFFFFFFF), 16)
   end,
   F = case get(fragment, Uri) of
          [] -> [];
          Rf -> integer_to_list(erlang:phash(Rf, 16#FFFFFFFF), 16)
   end,
   List = [
      atom_to_list(get(schema, Uri)),
      get(userinfo, Uri),
      get(host, Uri),
      integer_to_list(get(port, Uri)),
      get(path, Uri),
      Q,
      F
   ],
   lists:foldl(
      fun
         ([], Acc) -> Acc;
         ("/" ++ X, Acc) -> Acc ++ "/" ++ X;
         (X, Acc) -> Acc ++ "/" ++ X
      end, 
      [],
      List
   );      
to_path(Uri) ->
   to_path(new(Uri)).

   
%%
%% hash(Uri) -> binary()
%%
hash(Uri) ->
   crypto:sha(to_binary(Uri)).   
   


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
p_uri(":" ++ Str, schema, Acc, {S,A,P}) ->
   % TODO: binary_to_atom security OOM issue
   % Note: end-of-schema implies path
   p_uri(Str, path, "", {list_to_atom(Acc), A, P});

%% start of authority
p_uri("//" ++ Str, path, _, U) ->
   p_uri(Str, auth, "", U);   
   
%% start of path
p_uri("/" ++ Str, auth, Acc, {S,A,P}) ->
   p_uri(Str, path, "/", {S, Acc, P});   
   
%% accumulates token
p_uri([H | T], Tag, Acc, U) ->
   p_uri(T, Tag, Acc ++ [H], U);   
   
%% eof   
p_uri([], schema, "", {S,A,_}) ->
   {S,A,"/"};
p_uri([], schema, Acc, {S,A,P})  ->
   {S,A,Acc};
p_uri([], auth, Acc, {S,_,_}) ->
   % no path, set default
   {S, Acc, "/"};
p_uri([], path, Acc, {S,A,P}) ->
   {S, A, Acc}.
   

%%
%% authority   = [ userinfo "@" ] host [ ":" port ]
%%

%% end of userinfo
p_auth("@" ++ Str, host, Acc, {_, H, P}) ->
   p_auth(Str, host, "", {Acc, H, P});
   
%% end of host
p_auth(":" ++ Str, host, Acc, {U, _, P}) ->
   p_auth(Str, port, "", {U, Acc, P});
   
%% accumulates token
p_auth([H | T], Tag, Acc, A) ->
   p_auth(T, Tag, Acc ++ [H], A);

%% eof
p_auth([], host, Acc, {U, _, P}) ->
   {U, Acc, P};
p_auth([], port, Acc, {U, H, _}) ->
   {U, H, list_to_integer(Acc)}.
   
%%
%%   path: /over/there?name=ferret#nose
%%
p_path("?" ++ Str, path, Acc, {_, Q, F}) ->
   p_path(Str, 'query', "", {Acc, Q, F});
   
p_path("#" ++ Str, path, Acc, {_, Q, F}) ->
   p_path(Str, fragment, "", {Acc, Q, F});
   
p_path("#" ++ Str, 'query', Acc, {P, _, F}) ->
   p_path(Str, fragment, "", {P, Acc, F});
   
%% accumulates token
p_path([H | T], Tag, Acc, P) ->
   p_path(T, Tag, Acc ++ [H], P);   
   
%% eof
p_path([], path, Acc, {_, Q, F}) ->
   {Acc, Q, F};
p_path([], 'query', Acc, {P, _, F}) ->
   {P, Acc, F};
p_path([], fragment, Acc, {P, Q, _}) ->
   {P, Q, Acc}.
   
   

%%
%% Maps schema to default ports

schema_to_port(tcp,  [])  -> 80;   % custom schema for tcp sensors
schema_to_port(http,  []) -> 80;
schema_to_port(ssl,   []) -> 443;  % custom schema for ssl sensors 
schema_to_port(https, []) -> 443;
schema_to_port(_,     []) -> undefined;
schema_to_port(_,   Port) -> Port.
   

%%
%%
to_list(host, #uri{host = {N1, N2, N3, N4}}) ->
   integer_to_list(N1) ++ "." ++
   integer_to_list(N2) ++ "." ++
   integer_to_list(N3) ++ "." ++
   integer_to_list(N4);
to_list(host, #uri{host = H}) ->
   H;
to_list(authority, #uri{userinfo = [], host = []}) ->
   "";
to_list(authority, #uri{userinfo = [], port = undefined} = Uri) ->
   to_list(host, Uri);
to_list(authority, #uri{userinfo = U,  port = undefined} = Uri) ->
   U ++ "@" ++ to_list(host, Uri);   
to_list(authority, #uri{userinfo = [], port = P} = Uri) ->
   to_list(host, Uri) ++ ":" ++ integer_to_list(P);
to_list(authority, #uri{userinfo = U,  port = P} = Uri) ->   
   U ++ "@" ++ to_list(host, Uri) ++ ":" ++ integer_to_list(P);
to_list(resource, #uri{path = P, q = [], fragment = []}) ->
   P;
to_list(resource, #uri{path = P, q = [], fragment = F}) ->  
   P ++ "#" ++ F;
to_list(resource, #uri{path = P, q = Q, fragment = []}) ->
   P ++ "?" ++ Q;
to_list(resource, #uri{path = P, q = Q, fragment = F}) ->
   P ++ "?" ++ Q ++ "#" ++ F.


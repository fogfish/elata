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
-module(ek_uri_tests).
-author(dmitry.kolesnikov@nokia.com).
-include_lib("eunit/include/eunit.hrl").


uri_1_test() ->
   Uri = ek_uri:new("http://user@localhost:8080/p/a/t/h?query#frag"),
   ?assert(
      http =:= ek_uri:get(schema, Uri)
   ),
   ?assert(
      "user@localhost:8080" =:= ek_uri:get(authority, Uri)
   ),
   ?assert(
      "user" =:= ek_uri:get(userinfo, Uri)
   ),
   ?assert(
      "localhost" =:= ek_uri:get(host, Uri)
   ),
   ?assert(
      8080 =:= ek_uri:get(port, Uri)
   ),
   ?assert(
      "/p/a/t/h" =:= ek_uri:get(path, Uri)
   ),
   ?assert(
      "query" =:= ek_uri:get(q, Uri)
   ),
   ?assert(
      "frag" =:= ek_uri:get(fragment, Uri)
   ).
   
uri_2_test() ->
   Uri = ek_uri:new("http://user@localhost:8080/p/a/t/h?query"),
   ?assert(
      http =:= ek_uri:get(schema, Uri)
   ),
   ?assert(
      "user@localhost:8080" =:= ek_uri:get(authority, Uri)
   ),
   ?assert(
      "user" =:= ek_uri:get(userinfo, Uri)
   ),
   ?assert(
      "localhost" =:= ek_uri:get(host, Uri)
   ),
   ?assert(
      8080 =:= ek_uri:get(port, Uri)
   ),
   ?assert(
      "/p/a/t/h" =:= ek_uri:get(path, Uri)
   ),
   ?assert(
      "query" =:= ek_uri:get(q, Uri)
   ),
   ?assert(
      "" =:= ek_uri:get(fragment, Uri)
   ).   
   
uri_3_test() ->
   Uri = ek_uri:new("http://user@localhost:8080/p/a/t/h"),
   ?assert(
      http =:= ek_uri:get(schema, Uri)
   ),
   ?assert(
      "user@localhost:8080" =:= ek_uri:get(authority, Uri)
   ),
   ?assert(
      "user" =:= ek_uri:get(userinfo, Uri)
   ),
   ?assert(
      "localhost" =:= ek_uri:get(host, Uri)
   ),
   ?assert(
      8080 =:= ek_uri:get(port, Uri)
   ),
   ?assert(
      "/p/a/t/h" =:= ek_uri:get(path, Uri)
   ),
   ?assert(
      "" =:= ek_uri:get(q, Uri)
   ),
   ?assert(
      "" =:= ek_uri:get(fragment, Uri)
   ).      

uri_4_test() ->
   Uri = ek_uri:new("http://user@localhost:8080"),
   ?assert(
      http =:= ek_uri:get(schema, Uri)
   ),
   ?assert(
      "user@localhost:8080" =:= ek_uri:get(authority, Uri)
   ),
   ?assert(
      "user" =:= ek_uri:get(userinfo, Uri)
   ),
   ?assert(
      "localhost" =:= ek_uri:get(host, Uri)
   ),
   ?assert(
      8080 =:= ek_uri:get(port, Uri)
   ),
   ?assert(
      "/" =:= ek_uri:get(path, Uri)
   ),
   ?assert(
      "" =:= ek_uri:get(q, Uri)
   ),
   ?assert(
      "" =:= ek_uri:get(fragment, Uri)
   ).      
   
uri_5_test() ->
   Uri = ek_uri:new("http://user@localhost"),
   ?assert(
      http =:= ek_uri:get(schema, Uri)
   ),
   ?assert(
      "user@localhost:80" =:= ek_uri:get(authority, Uri)
   ),
   ?assert(
      "user" =:= ek_uri:get(userinfo, Uri)
   ),
   ?assert(
      "localhost" =:= ek_uri:get(host, Uri)
   ),
   ?assert(
      80 =:= ek_uri:get(port, Uri)
   ),
   ?assert(
      "/" =:= ek_uri:get(path, Uri)
   ),
   ?assert(
      "" =:= ek_uri:get(q, Uri)
   ),
   ?assert(
      "" =:= ek_uri:get(fragment, Uri)
   ).    
  
uri_6_test() ->
   Uri = ek_uri:new("http://localhost:8080"),
   ?assert(
      http =:= ek_uri:get(schema, Uri)
   ),
   ?assert(
      "localhost:8080" =:= ek_uri:get(authority, Uri)
   ),
   ?assert(
      "" =:= ek_uri:get(userinfo, Uri)
   ),
   ?assert(
      "localhost" =:= ek_uri:get(host, Uri)
   ),
   ?assert(
      8080 =:= ek_uri:get(port, Uri)
   ),
   ?assert(
      "/" =:= ek_uri:get(path, Uri)
   ),
   ?assert(
      "" =:= ek_uri:get(q, Uri)
   ),
   ?assert(
      "" =:= ek_uri:get(fragment, Uri)
   ).    
   
uri_7_test() ->
   Uri = ek_uri:new("http:/p/a/t/h"),
   ?assert(
      http =:= ek_uri:get(schema, Uri)
   ),
   ?assert(
      "" =:= ek_uri:get(authority, Uri)
   ),
   ?assert(
      "" =:= ek_uri:get(userinfo, Uri)
   ),
   ?assert(
      "" =:= ek_uri:get(host, Uri)
   ),
   ?assert(
      80 =:= ek_uri:get(port, Uri)
   ),
   ?assert(
      "/p/a/t/h" =:= ek_uri:get(path, Uri)
   ),
   ?assert(
      "" =:= ek_uri:get(q, Uri)
   ),
   ?assert(
      "" =:= ek_uri:get(fragment, Uri)
   ).    
   
urn_test() ->
   Uri = ek_uri:new("urn:p/a/t/h"),
   ?assert(
      urn =:= ek_uri:get(schema, Uri)
   ),
   ?assert(
      "" =:= ek_uri:get(authority, Uri)
   ),
   ?assert(
      "" =:= ek_uri:get(userinfo, Uri)
   ),
   ?assert(
      "" =:= ek_uri:get(host, Uri)
   ),
   ?assert(
      undefined =:= ek_uri:get(port, Uri)
   ),
   ?assert(
      "p/a/t/h" =:= ek_uri:get(path, Uri)
   ),
   ?assert(
      "" =:= ek_uri:get(q, Uri)
   ),
   ?assert(
      "" =:= ek_uri:get(fragment, Uri)
   ).    
   

uri_host_1_test() ->
   ?assert(
      "localhost" =:= ek_uri:get(host, "http://localhost:8080/path")
   ).
    
uri_host_2_test() ->
   ?assert(
      "localhost" =:= ek_uri:get(host, "http://localhost/path")
   ).
    
uri_host_3_test() ->
   ?assert(
      "localhost" =:= ek_uri:get(host, "http://user@localhost:8080/path")
   ).   
    
uri_host_4_test() ->
   ?assert(
      "" =:= ek_uri:get(host, "urn:path")
   ).
   
uri_port_1_test() ->
   ?assert(
      8080 =:= ek_uri:get(port, "http://localhost:8080/path")
   ).
    
uri_port_2_test() ->
   ?assert(
      8080 =:= ek_uri:get(port, "http://user@localhost:8080/path")
   ).   
    
uri_port_3_test() ->
   ?assert(
      undefined =:= ek_uri:get(port, "urn:path")
   ).   
   
uri_path_1_test() ->
   ?assert(
      "/path" =:= ek_uri:get(path, "http://localhost:8080/path")
   ).   
    
uri_path_2_test() ->
   ?assert(
      "/" =:= ek_uri:get(path, "http://localhost:8080/")
   ).   
    
uri_path_3_test() ->
   ?assert(
      "/" =:= ek_uri:get(path, "http://localhost:8080")
   ).   
    
uri_path_4_test() ->
   ?assert(
      "path" =:= ek_uri:get(path, "urn:path")
   ).   
    
uri_query_1_test() ->
   ?assert(
      "a=b" =:= ek_uri:get(q, "http://localhost:8080/path?a=b")
   ).
    
uri_query_2_test() ->
   ?assert(
      "a=b" =:= ek_uri:get(q, "http://localhost:8080/path?a=b#test")
   ).   
    
uri_frag_1_test() ->
   ?assert(
      "test" =:= ek_uri:get(fragment, "http://localhost:8080/path#test")
   ).
 
uri_frag_2_test() ->
   ?assert(
      "test" =:= ek_uri:get(fragment, "http://localhost:8080/path?a=b#test")
   ).    
   
uri_path_test() ->
   Uri = ek_uri:new("http://user@localhost:8080/p/a/t/h"),
   ?assert(
      "/p/a/t/h" =:= ek_uri:path(0, Uri)
   ),
   ?assert(
      "/a/t/h" =:= ek_uri:path(1, Uri)
   ),  
   ?assert(
      "/t/h" =:= ek_uri:path(2, Uri)
   ),
   ?assert(
      "/h" =:= ek_uri:path(3, Uri)
   ),
   ?assert(
      "/p/a/t" =:= ek_uri:path(-1, Uri)
   ),
   ?assert(
      "/p/a" =:= ek_uri:path(-2, Uri)
   ),
   ?assert(
      "/p" =:= ek_uri:path(-3, Uri)
   ).
   
   
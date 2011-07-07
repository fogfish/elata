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
-module(kvs_reg).
-author(dmitry.kolesnikov@nokia.com).

%%
%% ELATA: Registry of Key/Value Buckets
%%

-export([
   % public API
   start/0,
   start_bucket/1,
   define/2,
   register/2,
   unregister/1,
   resolve/1,
   is_notify/1
]).

-define(REGISTRY, ?MODULE).
-record(bucket, {
   name,     %% unique name
   notify,   %% send a notification
   pid,      %% pid
   opts      %% list of options
}).

%%
%% Initializes registry
start() ->
   ets:new(?REGISTRY, [public, named_table, {keypos, 2}]),
   ok.

%%
%% Instantiates bucket
start_bucket(Bucket) ->
   case ets:lookup(?REGISTRY, Bucket) of
      [R]  ->
         kvs_bucket_sup:create_bucket(Bucket, R#bucket.opts);
      []   ->
         %% fallback, no option for bucket is defined 
         kvs_reg:define(Bucket, []),
         kvs_bucket_sup:create_bucket(Bucket, [])
   end.

   
%%
%% Define new bucket
define(Bucket, Opts)  ->
   ets:insert_new(?REGISTRY,
      #bucket{
         name   = Bucket,
         notify = proplists:is_defined(notify, Opts),
         pid    = undefined,
         opts   = proplists:delete(notify, Opts)
   }).

%%
%% Register instance of repository
register(Name, Pid) ->
   case ets:lookup(?REGISTRY, Name) of
      [R]  ->
         R1 = R#bucket{pid=Pid},
         true = ets:insert(?REGISTRY, R1),
         ok;
      []   ->  {error, undefined}
   end.   

%%
%% Delete
unregister(_Name) ->
   % TODO: Implement
   ok.   
   
%%
%% Resolves 
resolve(Name) ->
   case ets:lookup(?REGISTRY, Name) of
      [#bucket{pid=Pid}]  ->
         case Pid of
            undefined -> {error, undefined};
            _         -> {ok, Pid}
         end;      
      []   ->  
         {error, undefined}
   end.

is_notify(Name) ->
   case ets:lookup(?REGISTRY, Name) of
      [#bucket{notify=Notify}] -> 
         Notify;
      [] -> 
         false
   end.


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
-module(ek_sup).
-behaviour(supervisor).
-author(dmitry.kolesnikov@nokia.com).

-export([
   % supervisor
   start_link/1,
   init/1,
   % api
   listen/1
]).

%%%
%%% Root supervisor of Erlang Cluster
%%%
start_link(Config) ->
   supervisor:start_link({local, ?MODULE}, ?MODULE, [Config]).
   
init([Config]) -> 
   {ok,
      {
         {one_for_one, 4, 1800},
         prot(Config) ++ event(Config) ++ listener(Config)
      }
   }.

%% used from ek:start   
listen(Config) ->
   [P|_] = listener(Config),
   supervisor:start_child(ek_sup, P).   
   

%%-------------------------------------------------------------------
%%
%% Process config
%%
%%-------------------------------------------------------------------

%% protocol stack
prot(Config) ->
   [
   %% socket based transport
   {
      ek_ws_sup,
      {
         ek_ws_sup,
         start_link,
         [Config]
      },
      permanent, 2000, supervisor, dynamic
   },
   %% cluster management protocol
   {
      ek_prot_sup,
      {
         ek_prot_sup,
         start_link,
         [Config]
      },
      permanent, 2000, supervisor, dynamic
   }
   ].
   
%% message listener
listener(Config) ->
   case proplists:get_value(node, Config) of
      undefined -> [];
      Uri       ->
         [{
            ek_ws,
            {
               ek_ws,
               start_link,
               [Config, {listen, Uri}]
            },
            permanent, 2000, worker, dynamic
         }]
   end.
   
%% event management
event(Config) ->
   [
   % gen_event notification 
   {
      ek_evt,
      {
         ek_evt,
         start_link,
         []
      },
      permanent, 2000, worker, dynamic
   },
   % message passing notification
   {
      ek_evt_srv,
      {
         ek_evt_srv,
         start_link,
         []
      },
      permanent, 2000, worker, dynamic
   }
   ].   
   

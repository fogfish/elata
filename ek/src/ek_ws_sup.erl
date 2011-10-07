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
-module(ek_ws_sup).
-author(dmitry.kolesnikov@nokia.com).

-behaviour(supervisor).

-export([
   % supervisor
   start_link/1,
   init/1,                    
   % api
   listen/1,
   accept/1,
   connect/1
]).


listen(Port) ->
   supervisor:start_child(?MODULE, [{listen, Port}]).

accept(Sock) ->
   supervisor:start_child(?MODULE, [{accept, Sock}]).
   
connect(Node) ->
   supervisor:start_child(?MODULE, [{connect, Node}]).
   
%%%------------------------------------------------------------------
%%%
%%% Supervisor
%%%
%%%------------------------------------------------------------------
start_link(Config) ->
   supervisor:start_link({local, ?MODULE}, ?MODULE, [Config]).
   
init([Config]) ->
   Process = {
      ek_ws,         % child id
      {
         ek_ws,      % Mod
         start_link, % Fun
         [Config]    % Args
      },
      transient, 2000, worker, dynamic 
   },
   {ok,
      {
         {simple_one_for_one, 2, 1},   % 2 faults per second
         [Process]
      }
   }.

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
-module(be_static_view).
-behaviour(gen_server).
-author(dmitry.kolesnikov@nokia.com).
-include("include/def.hrl").

%%
%% ELATA: Compiles uri view
%%

-export([
   % api
   start_link/2,
   % gen_server
   init/1, 
   handle_call/3,
   handle_cast/2, 
   handle_info/2, 
   terminate/2, 
   code_change/3 
]).

-record(srv, {
   scale,
   timeout,
   view
}).

-define(PROPERTY, [
   interlace,
   'slope-mode',
   'full-size-mode',
   rigid,
   {border, 0},
   {imgformat, 'PNG'},
   {'lower-limit', 0}
]).


%%
%% start
start_link(View, {Scale, Timeout}) ->
   gen_server:start_link(?MODULE, [View, Scale, Timeout], []).

init([View, Scale, Timeout]) ->
   timer:send_after(
      erlang:round(60000 + math:log(Timeout) * 1000), 
      timeout
   ),
   {ok,
      #srv{
         scale   = Scale,
         timeout = Timeout,
         view    = View
      }
   }.  
   
handle_call(_Req, _From, State) ->
   {reply, undefined, State}.
   
handle_cast(_Req, State) ->
   {noreply, State}.

handle_info(timeout, State) ->
   render(State#srv.view, State#srv.scale),
   timer:send_after(State#srv.timeout, timeout),
   {noreply, State};
handle_info(_Msg, State) ->
   {noreply, State}.
   
terminate(_Reason, _State) ->
   ok.
   
code_change(_OldVsn, State, _Extra) ->
   {ok, State}.   
   
%%%------------------------------------------------------------------
%%%
%%% Server implementation
%%%
%%%------------------------------------------------------------------
render(View, Scale) ->
   {ok, Uris}   = keyval_store:to_list(kv_job),
   {ok, Agents} = keyval_store:to_list(kv_agent),
   List = lists:map(
      fun({kvset, Id, _}) -> Id end,
      Agents
   ),
   lists:foreach(
      fun(Set) -> 
         Uri = kvset:name(Set),
         lists:foreach(
            fun(X) -> render_uri(X, Uri, View, Scale) end,
            List
            %%kvset:get(location, Set, [])
         )
      end,
      Uris
   ).

render_uri(Location, Uri, View, {Sdim, Sval}) ->
   Hash   = erlang:phash2(Uri),
   Prefix = lists:flatten(
      io_lib:format(
         '~.16x/~s',
         [Hash, "0x", Location]
      )
   ),
   Image  = lists:flatten(
      io_lib:format(
         '~.16x/~s/~s/~b.png',
         [Hash, "0x", Location, View#view.sys_id, scale_to_sec({Sdim, Sval})]
      )
   ),
   {Width, Height} = View#view.scale,
   Props = [{width, Width}, {height, Height}, {start, -1 * scale_to_sec({Sdim, Sval})}, {'end', now} | ?PROPERTY],
   tsa_db:render(Prefix, Image, View#view.ds, View#view.theme, Props),
   % update feed info
   {ok, Data0} = case keyval_store:lookup(kv_telemetry, Uri) of
      {error, _} -> {ok, kvset:new(Uri)};
      Result     -> Result
   end,               
   Data1 = kvset:set(
      [Location, view, View#view.sys_id], 
      scale_to_sec({Sdim, Sval}), 
      Data0
   ),
   keyval_store:insert(kv_telemetry, Data1),
   ok.


%% Converts a scale definition into values
scale_to_sec({Dim, Value}) ->
   case Dim of
      min   -> 60   * Value;                                
      hour  -> 3600 * Value;
      day   -> 3600 * 24 * Value;
      week  -> 3600 * 24 *   7 * Value;
      month -> 3600 * 24 *  30 * Value;
      year  -> 3600 * 24 * 365 * Value
   end.
   

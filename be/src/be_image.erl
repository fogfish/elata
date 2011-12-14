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
-module(be_image).
-behaviour(gen_server).
-author(dmitry.kolesnikov@nokia.com).

%%
%% Image worker (renders images based on statistic)
%%
%% Worker identity is uri
%%    http://{agent}/{key}#{template}
%%       agent    - node where measurments performed 
%%       key      - identity of process
%%       template - name of template file
%%

-export([
   start_link/3,
   %% gen_server
   init/1, 
   handle_call/3,
   handle_cast/2, 
   handle_info/2, 
   terminate/2, 
   code_change/3 
]).

-record(srv, {
   cat,       % name of category where key belongs
   key,       % key
   val,       % value
   name,      % template name
   host,      % host
   proc,      % process identity
   tpl        % compiled template
}).

%%
%% debug macro
-ifdef(DEBUG).
-define(DEBUG(M), error_logger:info_report([{?MODULE, self()}] ++ M)).
-else.
-define(DEBUG(M), true).
-endif.

%%
%%
start_link(Spec, Pid, Proc) ->
  gen_server:start_link(?MODULE, [Spec, Pid, Proc], []).
  
init([Spec, Id, Proc]) ->
   % register value (Spec is Category specification)
   Cat = proplists:get_value(uri, Spec),
   gen_kvs:val_init(Cat, Id),
   % start rendering timer(s)
   lists:foreach(
      fun({Scale, Timeout}) ->
         timer:send_after(
            % first timeout in range of mins to render all viewes
            erlang:round(60000 + math:log(Timeout) * 1000),
            {render, Scale, Timeout}
         )
      end,
      proplists:get_value(scale, Spec)
   ),
   % template id is fragment
   error_logger:info_report([{id, Id}]),
   Tname = proplists:get_value(template, Spec) ++ "/" ++ ek_uri:path(1, Id),
   {ok, 
      #srv{
         cat  = Spec, 
         key  = Id,
         val  = Proc,
         tpl  = be_rrd:compile(Tname)
      } 
   }. 
   
handle_call({kvs_put, Key, Val}, _From, S) ->
   {reply, ok, S#srv{val=Val}};
handle_call({kvs_get, Key}, _From, S) ->
   {reply, {ok, S#srv.val}, S};
handle_call(_Req, _From, State) ->
   {reply, undefined, State}.
   
handle_cast({kvs_remove, Key}, State) ->
   {stop, normal, State};
handle_cast(_Req, State) ->
   {noreply, State}.
 
handle_info({render, Scale, Timeout}, S) ->   
   ?DEBUG([{render, Scale}, {id, ek_uri:to_binary(S#srv.key)}]),
   % image rendering 
   Title = binary_to_list(proplists:get_value(script, S#srv.val)),
   Node  = ek_uri:get(authority, S#srv.key),
   Path  = ek_uri:to_path(
         ek_uri:set(path, ek_uri:path(-1, S#srv.key), S#srv.key)
   ),
   Ctx = [
      {title, Node ++ " " ++ Title},
      {from,  "-" ++ Scale},
      {trend, integer_to_list(erlang:round(scale_to_sec(Scale) / 3))},
      {path,  Path}
   ],
   ImgUri = ek_uri:append(path, "/" ++ Scale ++ ".png", S#srv.key), 
   be_rrd:render(S#srv.cat, Ctx, S#srv.tpl, ek_uri:to_path(ImgUri)),
   timer:send_after(Timeout, {render, Scale, Timeout}),
   {noreply, S};
handle_info(_Msg, S) ->
   {noreply, S}.
   
terminate(_Reason, S) ->
   Cat = proplists:get_value(uri, S#srv.cat),
   gen_kvs:val_terminate(Cat, S#srv.key),
   ok.
   
code_change(_OldVsn, State, _Extra) ->
   {ok, State}.     

%%%------------------------------------------------------------------   
%%%
%%% Private functions
%%%
%%%------------------------------------------------------------------

scale_to_sec("1hours")  ->  1 * 3600;
scale_to_sec("12hours") -> 12 * 3600;
scale_to_sec("1day")    -> 24 * 3600;
scale_to_sec("3days")   ->  3 * 24 * 3600;
scale_to_sec("1week")   ->  7 * 24 * 3600;
scale_to_sec("1month")  -> 30 * 24 * 3600;
scale_to_sec("6months") ->  6 * 30 * 24 * 3600;
scale_to_sec("1year")   -> 12 * 30 * 24 * 3600.




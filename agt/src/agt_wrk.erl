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
-module(agt_wrk).
-behaviour(gen_fsm).
-author(dmitry.kolesnikov@nokia.com).
-include("include/def.hrl").

%%
%% Generic process to handle probe operation
%%

-export([
   start_link/2,
   %% FSM
   'IDLE'/2, 
   'PROBE'/2,
   %% gen_fsm
   init/1,
   handle_info/3,
   handle_event/3,
   handle_sync_event/4,
   terminate/3,
   code_change/4
]).

%%
%% State 
%%
-record(fsm, {
   opts,       %% fsm opts proplists
   uri,        %% uri to probe
   req,        %% nbr of executed requests
   
   filter,    %% list of filters
   telemetry   %% list of telemetry
}). 

%%
%% Uri - Uri to measure
start_link(Opts, Uri) ->
   gen_fsm:start_link(?MODULE, [Opts, Uri], []).
   
%%
%% Init
init([Opts, Uri]) ->
   keyval_store:insert(kv_registry, {uid, Uri, self()}),
   %% delay probe by random time
	gen_fsm:send_event_after(
	   random:uniform( opt(idletime, Opts) ), 
	   probe
	),
	UObj   = uri_util:new(Uri),
	Filter = create_filters(UObj),
	State = #fsm{
	   opts = Opts,
	   uri  = UObj,
	   req  = 0,
	   filter    = Filter,
	   telemetry = [] 
   },
   {ok, 'IDLE', State}.

%%
%%
'IDLE'(probe, #fsm{opts = Opts} = State) ->
   % timer is expired job needs to be done
   gen_fsm:send_event_after( opt(thinktime, Opts), probe),
   {
      next_state, 
      'PROBE', 
      State#fsm{req = opt(cycle, Opts) }
   };
   
'IDLE'(_Evt, State) ->  
   {next_state, 'IDLE', State}.
   
%%
%%
'PROBE'(probe, #fsm{opts = Opts, uri = Uri, req = Req, 
        filter = Filter, telemetry = Tele} = State) -> 
   {ok, Raw} = execute_case(Uri, Opts),  
   % NTele = [],
   NTele1 = pass_filters(Filter, Raw, Tele),
   PassedReq = Req - 1,
   case PassedReq of
      0 ->
         % Flush telemetry into storage
         Stream = result_filters(Filter, NTele1, kvset:new( uri_util:to_binary(Uri) )),
         NTele2 = reset_filters(Filter,  NTele1),
         keyval_store:insert(kv_telemetry, Stream),
         gen_fsm:send_event_after(opt(idletime, Opts), probe),
         {next_state, 'IDLE', State#fsm{telemetry = NTele2}};
      _ ->
         gen_fsm:send_event_after(opt(thinktime, Opts), probe),
         {next_state, 'PROBE', State#fsm{req = PassedReq, telemetry = NTele1}}
   end;

'PROBE'(_Evt, State) ->
   {next_state, 'IDLE', State}.
      
handle_info(_Info, StateName, State) ->
   {next_state, StateName, State}.

handle_event(_Evt, StateName, State) ->   
   {next_state, StateName, State}.

handle_sync_event(_Evt, _From, StateName, State) ->
   {next_state, StateName, State}.
   
terminate(_Reason, _StateName, _State) ->
   ok.
   
code_change(_OldVsn, StateName, State, _Extra) ->
   {ok, StateName, State}.
   
%%   
%%
%update_probes([Probe | T], Dist, State, Result) ->
%   {Tag, Value} = Probe,
%   C  = proplists:get_value(Tag, Dist, la_counter:new(State#fsm.ewma)),
%   C1 = la_counter:add(C, Value),
%   update_probes(T, Dist, State, [{Tag, C1} | Result]);
%update_probes([], _Dist, _State, Result) ->
%   Result.
   
   
%%%------------------------------------------------------------------
%%%
%%% Private functions
%%%
%%%------------------------------------------------------------------

%% shortcut for proplists
opt(Key, List) -> 
   proplists:get_value(Key, List).
   
%% return filters configured for Uri   
create_filters(#uri{schema = _Schema} = _Uri) ->
   % TODO: configurable list
   [ 
      {mean, fun tsa_filter:moving_mean/2},
      {std,  fun tsa_filter:moving_std/2},
      {min,  fun tsa_filter:min/2},
      {max,  fun tsa_filter:max/2}
   ].

%% apply filters to raw data    
pass_filters(Filter, Raw, Tele) ->   
   % Pass filters throught raw data
   lists:map(
      % foreach measures component
      fun({Tag, RVal}) ->
         NTagTele = case opt(Tag, Tele) of
            undefined ->
               lists:map(
                  fun({_, Func}) -> Func(undefined, RVal) end,
                  Filter
               );
            TagTele ->
               lists:zipwith(
                  fun({_, Func}, F) -> Func(F, RVal) end,
                  Filter, TagTele
               )
         end,
         {Tag, NTagTele}
      end,
      Raw
   ).
  
%% extract results from filters   
result_filters(Filter, Tele, Stream) ->
   lists:foldl(
      fun({Tag, TagTele}, Acc) ->
         KVPair = lists:zipwith(
            fun({Fid, _}, F) -> 
               {Fid, tsa_filter:to_value(F)}
            end,
            Filter, TagTele
         ),
         lists:foldl(
            fun({K,V}, Acc1) -> kvset:set([Tag, K], V, Acc1) end,
            Acc,
            KVPair
         )
      end,
      Stream,
      Tele
   ).
   
%% reset filters for next round   
reset_filters(Filter, Tele) ->
   lists:map(
      fun({Tag, TagTele}) ->
         NTagTele = lists:zipwith(
            fun({_, Func}, F) -> Func(F, eof) end,
            Filter, TagTele
         ),
         {Tag, NTagTele}
      end,
      Tele
   ).
   
%% perform uri probe   
execute_case(#uri{schema = Schema} = Uri, Opts) ->
   case Schema of
     tcp   ->
        agt_probe:tcp(Uri, Opts);
     ssl   ->
        agt_probe:ssl(Uri, Opts);
     http  ->
        agt_probe:http(Uri, Opts);
     https ->
        agt_probe:http(Uri, Opts);
     _ ->
        {error, not_supported}
   end.
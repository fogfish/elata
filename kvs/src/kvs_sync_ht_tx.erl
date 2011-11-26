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
-module(kvs_sync_ht_tx).
-behaviour(gen_fsm).
-author(dmitry.kolesnikov@nokia.com).

%%%
%%% Bucket Sync TX based on hash tree
%%%   1. define TX end-point {node}/sync/ht/{TX} tx <- sha1({local node, remote node, now, cat})   
%%%   2. send sync_req {TX, Cat} to remote peer endpoint /sync/ht
%%%   3. if remote peer accepts the request then it send ht signature
%%%   4. normal ht reconsilation algorithm is started
%%% 

-export([
   start_link/0,
   start_link/1,
   start_link/2,
   % gen_fsm
   init/1,
   'LISTEN'/2, 
   'DIFF'/2,
   'DATA'/2,
   handle_event/3,
   handle_sync_event/4,
   handle_info/3,
   terminate/3,
   code_change/4
]).

%%
%% debug macro
-ifdef(DEBUG).
-define(DEBUG(M), error_logger:info_report([{?MODULE, self()}] ++ M)).
-else.
-define(DEBUG(M), true).
-endif.

-record(fsm, {
   tx,               % sync tx
   ht   = undefined, % hash tree
   keys = undefined  % keys to sync
   %c_msg, % counter of transmitted messages required to accomplish sync
   %c_byte % counter of transmitted bytes required to accomplish sync 
}).

%%%------------------------------------------------------------------   
%%%
%%% protocol message
%%%
%%%------------------------------------------------------------------

%% initiate sync transaction
-record(sync_tx, {
   type,   % transaction type
           %   p2p    - exchange of changed/missed keys between both peers 
           %   master - replicated changed/missed keys to slave peer
           %   slave  - remove changed/missed keys at slave peer
   uri     % identity of category to be synchronized 
}).

%% reconsilate
-record(sync_diff, {
   hash,      % signature at hash tree
   intersect  % signature intersection (hashes exists on both peers)
}).



-define(SYNC_URI,     <<"/kvs/sync/ht">>).
%% timeout: wait for remote peer to estmate diff on key set
-define(T_DIFF_WAIT,   60000). %% TODO: performance of ht needs to be validated
%% timeout: wait
-define(T_DATA_WAIT,    5000). 

%%
%% Listen of incoming sync requests
start_link() ->
   gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).  

%%
%% Sync category with node
start_link(Type, Uri) ->
   gen_fsm:start_link(?MODULE, [Type, Uri], []).

%%
%% Accept sync transaction
start_link(TX) ->
   gen_fsm:start_link(?MODULE, [TX], []).
   
init([]) ->
   ek:register({sync, undefined, ?SYNC_URI}),
   {ok, 
      'LISTEN', 
      #fsm{}
   };
  
%%
%% start sync transaction
init([Type, Uri]) ->
   prot_sync_req(Type, ek_uri:new(Uri)),
   ek:register({sync, undefined,  tx(Uri)}),
   {ok, 
      'DIFF',
      #fsm{
         tx = #sync_tx{
            type = Type,
            uri  = ek_uri:new(Uri)
         }
      },
      ?T_DIFF_WAIT
   };

%%
%% Accept a sync request
init([TX]) when is_record(TX, sync_tx) ->
   ek:register({sync, undefined, tx(TX#sync_tx.uri)}),
   gen_fsm:send_event_after(10, diff_init),
   ?DEBUG([{sync, sync_tx}, {tx, TX}]),
   {ok, 
      'DIFF', 
      #fsm{
         tx   = TX
      },
      ?T_DIFF_WAIT
   }.
   
%%%------------------------------------------------------------------   
%%%
%%% gen_fsm
%%%
%%%------------------------------------------------------------------

%%%
%%% 
'LISTEN'(TX, S) when is_record(TX, sync_tx) ->
   ?DEBUG([{sync, accept}, {tx, TX}]),
   case kvs_sync_ht_tx_sup:create(TX) of
      {ok, Pid} -> ok;
      _         -> prot_sync_stop(TX#sync_tx.uri)
   end,
   {next_state, 'LISTEN', S};
'LISTEN'(_Evt, S) ->
   {next_state, 'LISTEN', S}.


%%%
%%%
'DIFF'(timeout, S) ->
   {stop, {error, timeout}, S};
'DIFF'(sync_stop, S) ->
   {stop, normal, S};
'DIFF'(diff_init, #fsm{tx = Ltx} = S) ->
   % sync is accepted calculate diff for first time
   HT = ht(Ltx#sync_tx.uri),
   prot_sync_diff(Ltx#sync_tx.uri, HT),
   {next_state, 'DIFF', S#fsm{ht = HT}, ?T_DIFF_WAIT};
'DIFF'(Msg, #fsm{tx = TX, ht = undefined} = S) when is_record(Msg, sync_diff) -> 
   'DIFF'(Msg, S#fsm{ht = ht(TX#sync_tx.uri)});
'DIFF'(#sync_diff{hash=Phash, intersect = Pint}, #fsm{tx = TX, ht = HT} = S) ->
   case kvs_ht:depth(Phash) of
      % iteration over tree is completed
      -1 -> 
         ?DEBUG([{sync, iteration_over}, {tx, TX}]),
         {next_state, 'DATA', S#fsm{ht = kvs_ht:filter(Pint, HT)}, 0};
      % iteration over tree is not completed
      _  ->
         {Lhash, Lint, NHT} = ht_diff(Phash, Pint, HT),
         if
            % nothing to sync tree(s) are equal
            NHT =:= undefined ->
               prot_sync_stop(TX#sync_tx.uri),
               ?DEBUG([{sync, equal}, {tx, TX}]),
               {stop, normal, S#fsm{ht = NHT}};
            % negotiate diff
            true ->
               prot_sync_diff(TX#sync_tx.uri, Lhash, Lint),
               case kvs_ht:depth(Lhash) of 
                  -1 -> 
                     ?DEBUG([{sync, iteration_over}, {tx, TX}]),
                     {next_state, 'DATA', S#fsm{ht = NHT}, 0};
                  _ -> 
                     {next_state, 'DIFF', S#fsm{ht = NHT}, ?T_DIFF_WAIT}
               end
         end
   end; 
'DIFF'(_Evt, State) ->
   {next_state, 'DIFF', State}.
   
%%%
%%%
'DATA'(timeout, #fsm{tx = #sync_tx{type = p2p} = TX, ht = HT} = S) ->
   % p2p    - exchange of changed/missed keys between both peers
   xchange(TX#sync_tx.uri, HT),
   {stop, normal, S};
'DATA'(timeout, #fsm{tx = #sync_tx{type = master} = TX, ht = HT} = S) ->   
   % master - replicated changed/missed keys to slave peer
   xchange(TX#sync_tx.uri, HT),
   {stop, normal, S};
'DATA'(timeout, #fsm{tx = #sync_tx{type = slave} = TX, ht = HT} = S) ->   
   % slave  - remove changed/missed keys at slave peer
   Keys = kvs_ht:keys(HT),
   ?DEBUG([{sync, remove}, {key, Keys}]),
   {Scheme, _, Path} = TX#sync_tx.uri,
   lists:foreach(fun(K) -> kvs:remove({Scheme, undefined, Path}, K) end, Keys),
   {stop, normal, S};
'DATA'(_Msg, S) ->
   {stop, normal, S}.
   
%%%
%%% 
handle_event(Msg, Name, State) ->
   erlang:apply(?MODULE, Name, [Msg, State]).
   
handle_info(Msg, Name, State) ->
   erlang:apply(?MODULE, Name, [Msg, State]).

terminate(Reason, Name, S) ->
   ?DEBUG([{sync, done}, {tx, S#fsm.tx}]),
   ok.
   
code_change(_OldVsn, Name, State, _Extra) ->
   {ok, Name, State}.    
   
handle_sync_event(_Req, _From, Name, State) ->
   {reply, {error, not_supported}, Name, State}.   

%%%------------------------------------------------------------------   
%%%
%%% (N)-functions: implements (N)-entity capabilities
%%%
%%%------------------------------------------------------------------

%%
%% generates hash tree for category
ht({Schema, _, Path}) ->
   HT = kvs:fold(
      {Schema, undefined, Path}, 
      kvs_ht:new(), 
      fun(K, V, T) -> kvs_ht:insert(K, V, T) end
   ).
   
%%
%% calculates a local difference 
%%
%% ht_diff(Phash, Pint, HT) -> {Lhash, Lint, HT}
ht_diff(Phash, Pint, HT) ->
   NHT1   = kvs_ht:filter(Pint, HT),
   Lhash0 = kvs_ht:hash(Phash,  NHT1),
   Lint   = kvs_ht:diff(Phash,  Lhash0),
   NHT2   = kvs_ht:filter(Lint, NHT1),
   Lhash  = kvs_ht:hash(kvs_ht:depth(Lhash0) - 1, NHT1),
   {Lhash, Lint, NHT2}.
  
%%
%% exchanges key/vals
xchange({Scheme, _, Path} = Uri, HT) ->   
   Keys = kvs_ht:keys(HT),
   ?DEBUG([{sync, xchange}, {key, Keys}]),
   lists:foreach(
      fun(Key) ->
         {ok, Val} = kvs:get({Scheme, undefined, Path}, Key),
         ok = kvs:put(Uri, Key, Val)
      end, 
      Keys
   ).
   
   
%%%------------------------------------------------------------------   
%%%
%%% (N)-protocol: communcation behaviour of (N)-entity
%%%
%%%------------------------------------------------------------------

%%
%% invocates sync_tx on peer-(N)-entity, return TX 
prot_sync_req(Type, Uri) ->
   TX = #sync_tx{
      type = tx_inv_type(Type),
      uri  = {ek_uri:schema(Uri), ek:node(), ek_uri:path(Uri)}
   },
   ok = ek:send({sync, ek_uri:authority(Uri), ?SYNC_URI}, TX),  
   ?DEBUG([{sync, sync_tx}, {uri, Uri}, {tx, TX}]),
   ok.
   
%%
%% invocates sync_diff on peer-(N)-entity
prot_sync_diff(Uri, HT) ->
   Msg   = #sync_diff{
      hash      = kvs_ht:hash(kvs_ht:depth(HT), HT),
      intersect = {hash, kvs_ht:depth(HT), []}
   },
   ek:send(
      {
         sync,
         ek_uri:authority(Uri),
         tx({ek_uri:schema(Uri), ek:node(), ek_uri:path(Uri)})
      },
      Msg
   ),
   ?DEBUG([{sync, diff}, {uri, Uri}, {msg, Msg}]).
   
prot_sync_diff(Uri, Hash, Int) ->
   Msg = #sync_diff{
      hash      = Hash,
      intersect = Int
   },     
   ek:send(
      {
         sync,
         ek_uri:authority(Uri),
         tx({ek_uri:schema(Uri), ek:node(), ek_uri:path(Uri)})
      },
      Msg
   ),
   ?DEBUG([{sync, diff}, {uri, Uri}, {msg, Msg}]).
   
prot_sync_stop(Uri) ->
   ek:send(
      {
         sync,
         ek_uri:authority(Uri),
         tx({ek_uri:schema(Uri), ek:node(), ek_uri:path(Uri)})
      },
      sync_stop
   ).
   
%%%------------------------------------------------------------------   
%%% 
%%% Private
%%%
%%%------------------------------------------------------------------

%% 
%% transaction id
tx(Uri) ->
   TX = list_to_binary(
      bin_to_hex( 
         crypto:sha(
            term_to_binary(
               ek_uri:new(Uri)
            )
         )
      )
   ),
   <<?SYNC_URI/binary, "/", TX/binary>>.

%% inverts a transaction type  
tx_inv_type(p2p) ->
   p2p;
tx_inv_type(master) ->
   slave;
tx_inv_type(slave) ->
   master.
   
%%   
%%   
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
   
   

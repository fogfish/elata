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
-module(kvs_rrd).
-behaviour(gen_kvs).
-behaviour(gen_server).
-author(dmitry.kolesnikov@nokia.com).

%%
%% RRD key/val is round-robin bucket to collect time series data 
%% within rrd files, a dedicated file per key is created
%%    Key   is identity of data stream
%%    Value is time series stream value, in one of following format
%%       * integer(), float() then timestamp is calculated by erlang:now() 
%%       * tuple {timestamp, value}
%%       * list [{timestamp, ...}, {value, ...}] timestamp is optional
%%
%% Storage configuration parameters
%%    {datapath,  str} path to folder where *.rrd files are stored (default /var/lib/kvs/rrd)  
%%    {heartbeat, int} time interval between consequent measurments
%%    {type,      str} type of data stream (default GAUGE)
%%    {timeout,   int} data confidence time interval (default 3x heartbeat)
%%    
%%    {codepath,  str} prefix path to rrd tools (default /usr)
%%
%%    {iocache,  bool} run RRD with disk cache mode (default false)
%%    {sync,      int} disk sync timeout (default 120)
%%    {iotime,    int} time used to sync data on disk (default 60)

-export([
   start_link/1,
   % gen_kvs
   new/1,
   put/3,
   has/2,
   get/2,
   remove/2,
   handle/2
]).

%%
%% debug macro
-ifdef(DEBUG).
-define(DEBUG(M), error_logger:info_report([{?MODULE, self()}] ++ M)).
-else.
-define(DEBUG(M), true).
-endif.

%%%
%%% Internal server state
-record(srv, {
   code,   % path to RRD executables
   stream, % data stream parameters
   daemon, % {Host, Port} of RRD IO cache
   sock,   % open TCP/IP socket to cache 
   cache   % internal storage to keep interim key state
}).

-record(stream, {
   datadir,   % data dir prefix
   type,      % type of data stream
   heartbeat, % heartbeat 
   timeout
}).

start_link(Spec) ->
   gen_kvs:start_link(Spec).

new(Spec) ->
   % configure data stream storage
   Data  = proplists:get_value(datapath,  Spec,  "/var/lib/kvs/rrd"), 
   Hbeat = proplists:get_value(heartbeat, Spec, 60),
   Type  = proplists:get_value(type,      Spec, "GAUGE"),
   Tout  = proplists:get_value(timeout,   Spec, Hbeat * 3),
   % configure rrdtools
   Code  = proplists:get_value(codepath,  Spec, "/usr"),
   Cache = case proplists:is_defined(iocache, Spec) of
      true ->
         {Host, Port} = proplists:get_value(daemon, Spec, {"127.0.0.1", 42217}),
         W   = integer_to_list(proplists:get_value(sync,    Spec, 120)),
         Z   = integer_to_list(proplists:get_value(iotime,  Spec, 60)),
         P   = integer_to_list(Port),
         Cmd = Code ++ "/bin/rrdcached -g -w " ++ W ++ " -z " ++ Z ++ 
               " -l " ++ Host ++ ":" ++ P ++ " -B -b " ++ Data,
         erlang:open_port({spawn, Cmd}, [exit_status]),
         timer:send_after(5000, connect),
         ?DEBUG([{daemon, Cmd}]),
         {Host, Port};
      false ->
         error_logger:error_report({?MODULE, low_io_performance}),
         % internal design assumes low performance OS IPC if proxy in not used.
         undefined   
   end,
   % define in-memory bucket to keep a latest results of streams in memory
   {kvs, undefined, Path} = proplists:get_value(uri, Spec),
   Uri = {kvs, undefined, <<Path/binary, "#cache">>},
   {ok, _} = kvs:new(Uri, [{storage, kvs_ets}, direct]),
   ?DEBUG(Spec),
   {ok,
      #srv{
         code=Code,
         stream=#stream{
            datadir=Data, 
            type=Type, 
            heartbeat=Hbeat, 
            timeout=Tout
         },
         sock=undefined,
         daemon=Cache,
         cache=Uri
      }
   }.
   
   
%%%------------------------------------------------------------------
%%%
%%% gen_kvs
%%%
%%%------------------------------------------------------------------

%%
%% Put value into data stream
put(Key, {Timestamp, Value}, #srv{code = Code, stream = S, sock = Sock} = State) when is_integer(Value) ->
   DS = key_to_stream(Key),
   % validate that stream is present
   ok = case filelib:is_file(S#stream.datadir ++ DS) of
      true  -> ok;
      false -> create_stream(S#stream.datadir ++ DS, State)
   end,
   ok = case Sock of
      undefined ->
         % socket is not defined (io proxy daemon is not enabled/running)
         Cmd = [Code, "/bin/rrdupdate ", S#stream.datadir, DS, " ",  
                integer_to_list(Timestamp), ":", integer_to_list(Value)],
         rrdtool(Cmd);       
      _         ->
         % socket is exists, all data stream I/O handled by daemon
         % TODO: UPDATE ~s ~b:~f~n for float values is required
         Msg = lists:flatten(
            io_lib:format('UPDATE ~s ~b:~b~n', [DS, Timestamp, Value])
         ),
         gen_tcp:send(Sock, Msg)
   end,
   kvs:put(State#srv.cache, DS, {Timestamp, Value});   

put(Key, Val, State) when is_integer(Val) ->
   put(Key, {timestamp(), Val}, State);   

put(Key, Val, State) when is_list(Val) ->
   T = proplists:get_value(timestamp, Val, timestamp()),
   V = proplists:get_value(value,     Val),
   put(Key, {T, V}, State);

put(_Key, _Val, _State) ->
   {error, not_supported}.


%%
%%
has(Key, #srv{stream = S}) ->
   DS = key_to_stream(Key),
   filelib:is_file(S#stream.datadir ++ DS).

%%
%%   
get(Key, #srv{cache = Cache}) ->
   DS = key_to_stream(Key),
   case kvs:get(Cache, DS) of
      {ok, {_, Val}} -> {ok, Val};
      _              -> {error, not_found}
   end.

%%
%%
remove(_Pid, _S) ->
   {error, not_supported}.
   
%%
%% handle_info
handle(connect, #srv{daemon = {Host, Port}} = State) ->
   %% connect to cache daemon
   {ok, Sock}   = gen_tcp:connect(Host, Port, [binary, {packet, 0}]),
   ?DEBUG([{module, ?MODULE}, {daemon, {Host, Port}}]),
   State#srv{sock = Sock};
handle(_Msg, State) ->
   State.

   
%%%------------------------------------------------------------------
%%%
%%% Private function
%%%
%%%------------------------------------------------------------------   

%%
%% converts a key into file name
key_to_stream(Key) when is_list(Key) ->
   "/" ++ Key;
key_to_stream(Key) when is_binary(Key) ->
   "/" ++ binary_to_list(Key);
key_to_stream(Key) when is_atom(Key) ->
   "/" ++ atom_to_list(Key);
key_to_stream(Key) when is_tuple(Key) ->
   lists:flatten(lists:map(
      fun(X) -> 
         key_to_stream(X)
      end, 
      erlang:tuple_to_list(Key)
   )).
%
%key_to_stream(Key) ->
%   Hash = crypto:sha(term_to_binary(Key)),
%   Hex  = [ integer_to_list(X, 16) || X <- binary_to_list(Hash) ],
%   File = lists:append(Hex),
%   "/" ++ lists:sublist(File, 2) ++ "/" ++ File.
  
%%
%% retrive a time stamp
timestamp() ->
   {MSec, Sec, _} = erlang:now(),
   MSec * 1000000 + Sec.

%%
%% creates an data stream
create_stream(Stream, #srv{code = Code, stream = S}) ->
   filelib:ensure_dir(Stream),
   % TODO: make an dimension configurable via Bucket opts
   SPW   = erlang:round(7  * 24 * 3600 / S#stream.heartbeat), %steps per week
   SPQ   = erlang:round(15 * 60 / S#stream.heartbeat),        %steps in quater (15 min)
   QM    = erlang:round(31 * 24 * 60 / 15),      %quaters in month
   SPH   = erlang:round(30 * 60 / S#stream.heartbeat),        %steps in half
   HY    = erlang:round(365 * 24 * 60 / 30),     %halves in year
   CMD   = [Code, "/bin/rrdtool create ", Stream,
            " -s ", integer_to_list(S#stream.heartbeat)],
   DEF   = [" DS:val:", S#stream.type, ":", integer_to_list(S#stream.timeout), ":U:U "],
   RRA   = [" RRA:AVERAGE:0.9999:1:", integer_to_list(SPW),
            " RRA:AVERAGE:0.9999:",   integer_to_list(SPQ), ":", integer_to_list(QM),  %15min to 1 
            " RRA:AVERAGE:0.9999:",   integer_to_list(SPH), ":", integer_to_list(HY)],
   ?DEBUG([{stream, lists:append(CMD ++ DEF ++ RRA)}]),
   rrdtool(CMD ++ DEF ++ RRA).
      
   
%%   
%% executes command line RRD tool   
rrdtool(Cmd) ->
   case os:cmd(Cmd) of 
      "/bin/sh: line 1: " ++ ErrMsg ->
         error_logger:error_report([
            {rrdtools, ErrMsg},
            {command,  Cmd}
         ]),
         {error, ErrMsg};
      "ERROR: " ++ ErrMsg -> 
         error_logger:error_report([
            {rrdtools, ErrMsg},
            {command,  Cmd}
         ]),
         {error, ErrMsg};
      _  ->
         ok
   end.      

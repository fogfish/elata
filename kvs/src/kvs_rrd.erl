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
-author(dmitry.kolesnikov@nokia.com).

-behaviour(gen_server).
-behaviour(gen_kvs_bucket).

%%
%% RRD bucket is round-robin bucket to collect time series data
%%    Key  is identity of data stream
%%    Item is  value of data stream:
%%       * if value is integer() then timestamp is calculated by erlang:now() 
%%       * if value is tuple {timestamp, value}
%%       * if value is list [{timestamp, ...}, {value, ...}] timestamp is optional
%%
%% The bucket creates dedicates a RRD file for each data streams

-export([
   start_link/1,
   % gen_kvs_bucket
   construct/1,
   config/0,
   put/3,
   has/2,
   get/2,
   remove/2,
   % gen_server
   init/1, 
   handle_call/3, 
   handle_cast/2, 
   handle_info/2, 
   terminate/2, 
   code_change/3
]).

%%%
%%% Internal server state
-record(srv, {
   code,   % path to RRD executables
   stream, % data stream parameters
   daemon, % {Host, Port} of RRD IO cache
   sock    % open TCP/IP socket to cache  
}).

-record(stream, {
   datadir,   % data dir prefix
   type,      % type of data stream
   heartbeat, % heartbeat 
   timeout
}).

start_link(Bucket) ->
   % kvs_rrd bucket is singleton - one instance per node
   gen_server:start_link({local, ?MODULE}, ?MODULE, [Bucket], []).
   
%%%------------------------------------------------------------------
%%%
%%% gen_kvs_entity
%%%
%%%------------------------------------------------------------------
construct(_) ->
   {error, not_supported}.

config() ->
   [{supervise, worker}].
   
put(Pid, Key, Item) ->
   gen_server:cast(Pid, {kvs_put, Key, Item}).
   
has(Pid, Key) ->
   gen_server:call(Pid, {kvs_has, Key}).
   
get(Pid, Key) ->
   gen_server:call(Pid, {kvs_get, Key}).
   
remove(Pid, Key) ->
   gen_server:cast(Pid, {kvs_remove, Key}).   
   
   
%%%------------------------------------------------------------------
%%%
%%% gen_server
%%%
%%%------------------------------------------------------------------   
init([Bucket]) ->
   % configure data stream storage
   Data  = proplists:get_value(datapath, Bucket,  "/var/lib/kvs/rrd"), 
   Hbeat = proplists:get_value(heartbeat, Bucket, 60),
   Type  = proplists:get_value(type,      Bucket, "GAUGE"),
   Tout  = proplists:get_value(timeout,   Bucket, Hbeat * 3),
   % configure rrdtools
   Code  = proplists:get_value(codepath, Bucket, "/usr"),
   Cache = case proplists:is_defined(iocache, Bucket) of
      true ->
         {Host, Port} = proplists:get_value(daemon, Bucket, {"127.0.0.1", 42217}),
         W  = integer_to_list(proplists:get_value(flush,   Bucket, 120)),
         Z  = integer_to_list(proplists:get_value(iotime,  Bucket, 60)),
         P  = integer_to_list(Port),
         Cmd = Code ++ "/bin/rrdcached -g -w " ++ W ++ " -z " ++ Z ++ 
               " -l " ++ Host ++ ":" ++ P ++ " -B -b " ++ Data,
         erlang:open_port({spawn, Cmd}, [exit_status]),
         error_logger:info_report([{daemon, Cmd}]),
         timer:send_after(5000, connect),
         {Host, Port};
      false ->
         % internal design assumes low performance OS IPC if proxy in not used.
         undefined   
   end,
   % define in-memory bucket to keep a latest results of streams in memory
   ok = kvs_bucket:define(kvs_rrd_cache, [{storage, kvs_sys}]),
   % register itself
   Name = proplists:get_value(name, Bucket),
   ok   = kvs:put(kvs_sys_ref, Name, self()),
   {ok,
      #srv{
         code=Code,
         stream=#stream{datadir=Data, type=Type, heartbeat=Hbeat, timeout=Tout},
         sock=undefined,
         daemon=Cache
      }
   }.

%%
%% handle_call
handle_call({kvs_has, Key}, _From, State) ->
   {reply, impl_has(Key, State), State};
handle_call({kvs_get, Key}, _From, State) ->
   {reply, impl_get(Key), State};
handle_call(_Req, _From, State) ->
   {reply, undefined, State}.

%%
%% handle_cast
handle_cast({kvs_put, Key, Item}, State) ->
   impl_put(Key, Item, State),
   {noreply, State};
handle_cast({kvs_remove, Key}, State) ->
   {noreply, State};
handle_cast(_Req, State) ->
   {noreply, State}.

%%
%% handle_info
handle_info(connect, #srv{daemon = {Host, Port}} = State) ->
   %% connect to cache daemon
   {ok, Sock}   = gen_tcp:connect(Host, Port, [binary, {packet, 0}]),
   error_logger:info_report([{module, ?MODULE}, {daemon, {Host, Port}}]),
   {noreply, State#srv{sock = Sock}};
handle_info(_Msg, State) ->
   {noreply, State}.

%%
%% terminate
terminate(_Reason, _State) ->
   ok.
   
%%
%%
code_change(_OldVsn, State, _Extra) ->
   {ok, State}.
   
   
%%%------------------------------------------------------------------
%%%
%%% Private function
%%%
%%%------------------------------------------------------------------   

%%
%% put value into data stream
impl_put(Key, {Timestamp, Value}, #srv{code = Code, stream = S, sock = Sock} = State) when is_integer(Value) ->
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
         Msg = lists:flatten(
            io_lib:format('UPDATE ~s ~b:~f~n', [DS, Timestamp, Value])
         ),
         gen_tcp:send(Sock, Msg)
   end,
   ok = kvs:put(kvs_rrd_cache, DS, {Timestamp, Value});   

impl_put(Key, Item, State) when is_integer(Item) ->
   impl_put(Key, {timestamp(), Item}, State);   

impl_put(Key, Item, State) when is_list(Item) ->
   T = proplists:get_value(timestamp, Item, timestamp()),
   V = proplists:get_value(value, Item),
   impl_put(Key, {T, V}, State);

impl_put(Key, Item, State) ->
   {error, not_supported}.

   
%%
%%
impl_has(Key, #srv{stream = S}) ->
   DS = key_to_stream(Key),
   filelib:is_file(S#stream.datadir ++ DS).
   
impl_get(Key) ->
   DS = key_to_stream(Key),
   case kvs:get(kvs_rrd_cache, DS) of
      {ok, {_, Value}} -> {ok, Value};
      _                -> {error, not_found}
   end.
   
%%
%% converts a key into file name
key_to_stream(Key) ->
   Hash = crypto:sha(term_to_binary(Key)),
   Hex  = [ integer_to_list(X, 16) || X <- binary_to_list(Hash) ],
   File = lists:append(Hex),
   "/" ++ lists:sublist(File, 2) ++ "/" ++ File.
   
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
            " RRA:AVERAGE:0.9999:", integer_to_list(SPQ), ":", integer_to_list(QM),  %15min to 1 
            " RRA:AVERAGE:0.9999:", integer_to_list(SPQ), ":", integer_to_list(HY)],
   error_logger:info_report([{stream, lists:append(CMD ++ DEF ++ RRA)}]),
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

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
-module(tsa_db).
-behaviour(gen_server).
-author(dmitry.kolesnikov@nokia.com).

%% TODO conver into FSM due to connectivity with external process

-export([
  % api
  start_link/1,
  create/1,
  write/2,
  render/5,
  % gen_server
  init/1, 
  handle_call/3, 
  handle_cast/2, 
  handle_info/2, 
  terminate/2, 
  code_change/3 
]).

%% server state
-record(srv,  {
   code_path,
   data_path,
   template,
   sock,
   port

}).

start_link(Config) ->
   gen_server:start_link({local, ?MODULE}, ?MODULE, [Config], []).
   

init([Config]) ->
   %% spawn
   CodePath = proplists:get_value(codepath, Config),
   DataPath = proplists:get_value(datapath, Config),
   spawn_db_daemon(CodePath, DataPath),
   %% discover a path to template database file
   {file, Module} = code:is_loaded(?MODULE),
   Template = filename:dirname(filename:dirname(Module)) ++ "/priv/template.rrd",
   %% connect to rrd cached after 5 sec (daemon process is fully started)
   timer:send_after(5000, connect),
   {ok,
      #srv{
         data_path = DataPath,
         code_path = CodePath,
         template  = Template,
         sock      = undefined
      }
   }.
 
handle_call({create, Storage}, _From, State) ->
   {reply, srv_create(Storage, State), State};   
handle_call({render, Prefix, Image, Ds, Theme, Props}, _From, State) ->
   srv_render(Prefix, Image, Ds, Theme, State, Props),
   {reply, ok, State};    
handle_call(_Req, _From, State) ->
   {reply, undefined, State}.

handle_cast({create, Stream}, State) ->
   srv_create(Stream, State),
   {noreply, State};
handle_cast({write, Stream, Value}, State) ->
   srv_write(Stream, Value, State),
   {noreply, State};
handle_cast({render, Prefix, Image, Ds, Theme, Props}, State) ->
   srv_render(Prefix, Image, Ds, Theme, State, Props),
   {noreply, State};   
handle_cast(_Req, State) ->
   {noreply, State}.
   
   
handle_info(connect, State) ->
   {ok, Sock}   = gen_tcp:connect("127.0.0.1", 42217, [binary, {packet, 0}]),
   {noreply, State#srv{sock = Sock}};
handle_info({tcp, _Sock, _Msg}, State) ->  
   {noreply, State};
handle_info({tcp_closed, _Sock}, State) ->
   %% db daemon is terminated, dies
   {stop, deamon_crash, State};
handle_info(Msg, State) ->
   io:format('got msg: ~p~n', [Msg]),
   {noreply, State}.
   
terminate(_Reason, _State) ->
   ok.
   
code_change(_OldVsn, State, _Extra) ->
   {ok, State}.


%%%------------------------------------------------------------------
%%%
%%%  Server Implementation
%%%
%%%------------------------------------------------------------------

%%
%% create table if does not exist
create(Stream) ->
   gen_server:call(?MODULE, {create, Stream}). 

%srv_create(Storage, #srv{code_path = Cpath, data_path = Dpath, table = Table}) ->
srv_create(Stream, #srv{data_path = Dpath, template = Tpl}) ->
   Filename = Dpath ++ "/" ++ Stream,
   case filelib:is_file(Filename) of
      true  -> 
         ok;
      false ->
         % copy template
         filelib:ensure_dir(Filename),
         {ok, _} = file:copy(Tpl, Filename),
         ok
   end.
   
%%
%% writes data to table
write(Stream, Value) ->
   gen_server:cast(?MODULE, {write, Stream, Value}).
srv_write(_, _, #srv{sock = undefined}) ->   
   %% not connected yet, nothing todo
   ok;
srv_write(Stream, {Timestamp, Value}, #srv{sock = Sock} = State) ->
   srv_create(Stream, State),
   Msg = lists:flatten(
      io_lib:format('UPDATE ~s ~b:~f~n', [Stream, Timestamp, Value / 1000000])
   ),
   ok  = gen_tcp:send(Sock, Msg).

%%
%% render
render(Prefix, Image, Ds, Theme, Props) ->
   gen_server:call(?MODULE, {render, Prefix, Image, Ds, Theme, Props}).
srv_render(Prefix, Image, Ds, Theme, #srv{code_path = Cpath, data_path = Dpath, sock = Sock}, Props) ->
   Filename = Dpath ++ "/" ++ Image,
   Idef  = ds_to_rrd(Dpath, Prefix, Ds, Sock),
   Iprop = props_to_rrd(Props),
   Ivis  = theme_to_rrd(Theme),
   Cmd = [
      Cpath, "/bin/rrdtool graph ", Filename, Iprop, Idef, Ivis
   ],
   %%io:format('~s~n', [Cmd]),
   filelib:ensure_dir(Filename),
   exec_rrd(Cmd).

ds_to_rrd(Dpath, Prefix, Ds, Sock) ->
   Def = lists:map(
      fun ({Uid, Class, Attr}) ->
         %% PURE HACK: rrdcached do not support abs path via socket
         %%            explisit flush
         RRDFile = Prefix ++ "/" ++ atom_to_list(Class) ++ "/" ++ atom_to_list(Attr) ++ ".rrd",
         Msg = lists:flatten(
            io_lib:format('FLUSH ~s~n', [RRDFile])
         ),
         ok  = gen_tcp:send(Sock, Msg),
         " DEF:" ++ atom_to_list(Uid) ++ "=" ++ Dpath ++ "/" ++ RRDFile ++ ":value:AVERAGE"
      end,
      Ds
   ) ,
   {Cdef, _} = lists:mapfoldl(
      fun
         ({Uid, _Class, _Attr}, undefined) ->
            {" ", atom_to_list(Uid)};
         ({Uid, _Class, _Attr}, Acc) ->
            {" CDEF:a" ++ atom_to_list(Uid) ++ "=" ++ Acc ++ "," ++ atom_to_list(Uid) ++ ",+", "a" ++ atom_to_list(Uid) }
      end,
      undefined,
      Ds
   ),
   Def ++ Cdef.   

props_to_rrd(Props) ->
   lists:map(
      fun
         ({K, V}) when is_list(V) -> 
            " --" ++ atom_to_list(K) ++ " \"" ++ V ++ "\"";
         ({K, V}) when is_atom(V) ->
            " --" ++ atom_to_list(K) ++ " " ++ atom_to_list(V);   
         ({K, V}) when is_integer(V) ->
            " --" ++ atom_to_list(K) ++ " " ++ io_lib:format('~b', [V]);
         ({K, V}) when is_float(V) ->
            " --" ++ atom_to_list(K) ++ " " ++ io_lib:format('~f', [V]);
         (K) when is_atom(K) ->
            " --" ++ atom_to_list(K)
      end,
      Props
   ).   
   
theme_to_rrd(Theme) ->
   lists:map(
      fun(X) ->
         case X of
            {Var, no_title,  Line, Area} ->
               Id = atom_to_list(Var),
               " AREA:"  ++ Id ++ Area ++ ":" ++
               " LINE1:" ++ Id ++ Line;
            {Var, Title, Line, Area} ->
               Id  = atom_to_list(Var),
               SId = var_to_id(atom_to_list(Var)),
               " AREA:"  ++ Id ++ Area ++ ":" ++ "\"" ++ Title ++ "\"" ++
               " LINE1:" ++ Id ++ Line ++
               " VDEF:"  ++ Id ++ "min=" ++ SId ++ ",MINIMUM " ++
               " VDEF:"  ++ Id ++ "avg=" ++ SId ++ ",AVERAGE " ++
               " VDEF:"  ++ Id ++ "std=" ++ SId ++ ",STDEV "   ++
               " VDEF:"  ++ Id ++ "max=" ++ SId ++ ",MAXIMUM " ++
               " GPRINT:"++ Id ++ "min:\"min %1.3lf\" " ++
               " GPRINT:"++ Id ++ "avg:\"avg %1.3lf\" " ++
               " GPRINT:"++ Id ++ "std:\"+/- %1.3lf\" " ++
               " GPRINT:"++ Id ++ "max:\"max %1.3lf\"\\\\l";
            {Var, Title, Line} ->
               Id  = atom_to_list(Var),
               SId = var_to_id(atom_to_list(Var)),
               " LINE1:" ++ Id ++ Line ++ ":" ++ "\"" ++ Title ++ "\"" ++
               " VDEF:"  ++ Id ++ "min=" ++ SId ++ ",MINIMUM " ++
               " VDEF:"  ++ Id ++ "avg=" ++ SId ++ ",AVERAGE " ++
               " VDEF:"  ++ Id ++ "std=" ++ SId ++ ",STDEV "   ++
               " VDEF:"  ++ Id ++ "max=" ++ SId ++ ",MAXIMUM " ++
               " GPRINT:"++ Id ++ "min:\"min %1.3lf\" " ++
               " GPRINT:"++ Id ++ "avg:\"avg %1.3lf\" " ++
               " GPRINT:"++ Id ++ "std:\"+/- %1.3lf\" " ++
               " GPRINT:"++ Id ++ "max:\"max %1.3lf\"\\\\l";
            {Var, Title} ->
               Id  = atom_to_list(Var),
               SId = var_to_id(atom_to_list(Var)), 
               " VDEF:"  ++ Id ++ "min=" ++ SId ++ ",MINIMUM " ++
               " VDEF:"  ++ Id ++ "avg=" ++ SId ++ ",AVERAGE " ++
               " VDEF:"  ++ Id ++ "std=" ++ SId ++ ",STDEV "   ++
               " VDEF:"  ++ Id ++ "max=" ++ SId ++ ",MAXIMUM " ++
               " GPRINT:"++ Id ++ "min:\"" ++ Title ++ " min %1.3lf\" " ++
               " GPRINT:"++ Id ++ "avg:\"avg %1.3lf\" " ++
               " GPRINT:"++ Id ++ "std:\"+/- %1.3lf\" " ++
               " GPRINT:"++ Id ++ "max:\"max %1.3lf\"\\\\l"
         end   
      end,
      Theme
   ).
   
   
%%%------------------------------------------------------------------
%%%
%%%  Private function
%%%
%%%------------------------------------------------------------------

%% Convers human-readable atom to second
%to_sec(Time) ->
%   case Time of
%      min    -> 60;
%      quater -> 60 * 15;
%      half   -> 60 * 30;
%      hour   -> 3600;
%      day    -> 3600 * 24;
%      week   -> 3600 * 24 * 7;
%      month  -> 3600 * 24 * 30;
%      year   -> 3600 * 24 * 365;
%      Val    -> Val
%   end.   

var_to_id([$a]) ->
   "a";
var_to_id([$a | Id]) -> 
   Id;
var_to_id(Id) ->
   Id.
   
%%
spawn_db_daemon(CodePath, DataPath) ->
   erlang:open_port(
      {spawn, CodePath ++ "/bin/rrdcached -g -w 120 -z 60 -l 127.0.0.1:42217 -B -b " ++ DataPath}, 
      [exit_status]
   ).
   
%% executes RRD tool   
exec_rrd(Cmd) ->
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

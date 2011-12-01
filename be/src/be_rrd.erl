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
-module(be_rrd).
-author(dmitry.kolesnikov@nokia.com).

%%
%% RRD rendering pipeline
%%
-export([
   compile/1,
   render/4
]).


%%
%% compiles RRD template
compile(File) ->
   {ok, T} = file:read_file(File),
   c(binary_to_list(T), [], []).

%%
%% renders compiled template
render(Config, Ctx, T, File) ->
   Code = proplists:get_value(codepath,  Config),
   Data = proplists:get_value(datapath,  Config),
   NCtx = [{data, Data} | Ctx],
   Filename = Data ++ "/view/" ++ File,
   filelib:ensure_dir(Filename),
   Cmd  = [Code, "/bin/rrdtool graph ", Filename, " "] ++ T(NCtx),
   rrdtool(lists:flatten(Cmd)).

%%%------------------------------------------------------------------
%%%
%%% Private
%%%
%%%------------------------------------------------------------------

%%
c("<?" ++ T, Acc, R) ->
   Out = lists:reverse(Acc),
   c(T, [], [fun(_)-> Out end | R]);
c("?>" ++ T, Acc, R) ->
   Var = lists:reverse(Acc),
   c(T, [], [fun(C)-> proplists:get_value(list_to_atom(Var), C, "") end | R]);
c("/*" ++ T, Acc, R) ->
   Out = lists:reverse(Acc),
   c(T, [], [fun(_)-> Out end | R]);
c("*/" ++ T, Acc, R) ->
   c(T, [], R);
c("\n" ++ T, Acc, R) ->
   c(T, [" " | Acc], R);
c("\r\n" ++ T, Acc, R) ->
   c(T, [" " | Acc], R);   
c([H | T], Acc, R) ->
   c(T, [H | Acc], R);
c([], [], R) ->
   fun(X) ->   
      lists:flatten(lists:foldr(
         fun(F, Acc) -> Acc ++ F(X) end,
         "",
         R
      ))
   end;
c([], Acc, R) ->
   Out = lists:reverse(Acc),
   c([], [], [fun(_)-> Out end | R]).
   

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


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
-module(fe_webapp_api).
-author(dmitry.kolesnikov@nokia.com).
-include("include/def.hrl").

-export([
   resources/0,
   content_types/0
]).

%% end-points
-export([
   websrv/1,
   config/1,
   user/1,
   usecase/1,
   image/1,
   
   as_is/1
]).

%%
%% List of supported resources
resources() ->
   [
      {"r",  "/config/$",                fun ?MODULE:config/1},
      {"cr", "/user/$",                  fun ?MODULE:user/1},
      {"rcd","/user/$/service/$/usecase/$", fun ?MODULE:usecase/1},
      {"r",  "/user/$/service/$/usecase/$/location/$/image/$/scale/$", fun ?MODULE:image/1},
      {"r",  "/*",                       fun ?MODULE:websrv/1}
   ].
   
content_types() ->
   [
      {"text/plain", fun rest_mime_util:text_plain/1},
      {"application/erlang-base64", fun rest_mime_util:erlang_base64/1},
      {"text/html",  fun ?MODULE:as_is/1}
   ].
   
%%%------------------------------------------------------------------   
%%%
%%% WebApp
%%%
%%%------------------------------------------------------------------   
websrv({read, Path, Req}) ->
   Filename = case Path of
      []   -> webroot_dir() ++ "index.html";
      File -> webroot_dir() ++ File
   end,
   {ok, Bin} = file:read_file(Filename),
   case filename:extension(Filename) of
      ".html" -> {ok, Bin, "text/html"};
      ".css"  -> {ok, Bin, "text/css"};
      ".png"  -> {ok, Bin, "image/png"};
      ".jpg"  -> {ok, Bin, "image/jpeg"};
      ".js"   -> {ok, Bin, "application/javascript"}
   end.

%% return root_dir
webroot_dir() ->
   {file, Module} = code:is_loaded(?MODULE),
   filename:dirname(filename:dirname(Module)) ++ "/priv/webapp/".   
   
as_is({encode, Msg}) ->
   Msg;
as_is({decode, Msg}) ->
   Msg.
   
   
%%%------------------------------------------------------------------   
%%%
%%%   /config/$
%%%
%%%------------------------------------------------------------------   
config({read, _Path, Req}) ->
   %% TODO: check cross application config
   case dict:fetch(config, Req) of
      "location" ->
         keyval_store:to_list(kv_agent)
   end.      

%%%------------------------------------------------------------------   
%%%
%%%   /user/$
%%%
%%%------------------------------------------------------------------   
user({read, _Path, Req}) ->
   Uid = list_to_binary(dict:fetch(user, Req)),
   {ok, User} = keyval_store:lookup(kv_user, Uid),
   %% enumerate service (TODO: simplify function)
   R = lists:foldl(
      fun(Sid, Acc) ->
         % enumerate use-case of service
         lists:foldl(
            fun(Cid, Acc) ->
               Uri = kvset:get([<<"service">>, Sid, <<"usecase">>, Cid, <<"uri">>], Acc),
               case keyval_store:lookup(kv_telemetry, Uri) of
                  {error, _} ->
                     Acc;
                  {ok, Tele} ->
                     % maps mean uri telemetry to result set
                     T = lists:map(
                        fun(Lid) -> 
                           {Lid, kvset:get([Lid, telemetry, uri, mean], Tele)} 
                        end,
                        kvset:keys(Tele)
                     ),
                     kvset:set([<<"service">>, Sid, <<"usecase">>, Cid, telemetry], T, Acc)
               end
            end,
            Acc,
            kvset:keys([<<"service">>, Sid, <<"usecase">>], Acc)
         )
      end,
      User,
      kvset:keys([<<"service">>], User)
   ),
   {ok, R};
   
   %%{ok, User} = keyval_store:lookup(kv_user, Uid),
   %%lookup_telemetry(User);
   
user({create, _Path, Req, {kvset, _, UData}}) ->
   Uid  = list_to_binary(dict:fetch(user, Req)),
   User = {kvset, Uid, UData},
   keyval_store:create(kv_user, User),
   %% enumerate service (TODO: simplify function)
   %% enumerate service (TODO: simplify function)
   R = lists:foldl(
      fun(Sid, Acc) ->
         % enumerate use-case of service
         lists:foldl(
            fun(Cid, Acc) ->
               Uri = kvset:get([<<"service">>, Sid, <<"usecase">>, Cid, <<"uri">>], Acc),
               case keyval_store:lookup(kv_telemetry, Uri) of
                  {error, _} ->
                     Acc;
                  {ok, Tele} ->
                     % maps mean uri telemetry to result set
                     T = lists:map(
                        fun(Lid) -> 
                           {Lid, kvset:get([Lid, telemetry, uri, mean], Tele)} 
                        end,
                        kvset:keys(Tele)
                     ),
                     kvset:set([<<"service">>, Sid, <<"usecase">>, Cid, telemetry], T, Acc)
               end
            end,
            Acc,
            kvset:keys([<<"service">>, Sid, <<"usecase">>], Acc)
         )
      end,
      User,
      kvset:keys([<<"service">>], User)
   ),
   {ok, R}. 

%%%------------------------------------------------------------------   
%%%
%%%   /user/$/service/$/usecase/$
%%%
%%%------------------------------------------------------------------   
usecase({read, _Path, Req}) ->
   Uid = list_to_binary(dict:fetch(user,    Req)),
   Sid = list_to_binary(dict:fetch(service, Req)),
   Cid = list_to_binary(dict:fetch(usecase, Req)),
   {ok, User} = keyval_store:lookup(kv_user, Uid),
   % get service
   case kvset:get([<<"service">>, Sid, <<"usecase">>, Cid, <<"uri">>], User) of
      false ->
         {error, not_found};
      Uri   ->
         keyval_store:lookup(kv_telemetry, Uri)
   end;
   %
   %Usecase    = user:get_usecase(Cid, Sid, User),
   %{ok, Feed} = keyval_store:lookup(kv_feed, Usecase#usecase.uri),
   %{ok, {Usecase#usecase.sys_id, Usecase#usecase.dc_title, Feed}};
   
usecase({create, _Path, Req, {kvset, _, Input}}) ->
   Uid = list_to_binary(dict:fetch(user,    Req)),
   Sid = list_to_binary(dict:fetch(service, Req)),
   Cid = list_to_binary(dict:fetch(usecase, Req)),
   InData = {kvset, none, Input},
   {ok, User} = keyval_store:lookup(kv_user, Uid),
   case kvset:get([<<"service">>, Sid, <<"usecase">>, Cid], User) of
      false   ->
         % create new
         Uri    = kvset:get(<<"uri">>, InData),
         NUser0 = kvset:set([<<"service">>, Sid, title], kvset:get(<<"service">>, InData), User),
         NUser1 = kvset:set([<<"service">>, Sid, <<"usecase">>, Cid, <<"title">>], kvset:get(<<"usecase">>, InData), NUser0),
         NUser2 = kvset:set([<<"service">>, Sid, <<"usecase">>, Cid, <<"uri">>], Uri, NUser1),
         keyval_store:insert(kv_user, NUser2),
         case keyval_store:lookup(kv_job, Uri) of
            {error, not_found} ->
               Job0 = kvset:new(Uri),
               Job1 = kvset:set([<<"ua">>], kvset:get(<<"ua">>, InData), Job0),
               keyval_store:create(kv_job, Job1);
            _  ->
               % job exists do nothing
               ok
         end;
      Usecase ->
         % error exists
         {error, already_exists}
   end,
   ok.
   
   
%%%------------------------------------------------------------------   
%%%
%%%   /user/$/service/$/usecase/$/location/$/image/$/scale/$
%%%
%%%------------------------------------------------------------------   
image({read, Path, Req}) ->
   Uid = list_to_binary(dict:fetch(user,    Req)),
   Sid = list_to_binary(dict:fetch(service, Req)),
   Cid = list_to_binary(dict:fetch(usecase, Req)),
   Lid = dict:fetch(location, Req),
   Vid = dict:fetch(image,   Req),
   Did = dict:fetch(scale,   Req),
   {ok, User} = keyval_store:lookup(kv_user, Uid),
   Uri  = kvset:get([<<"service">>, Sid, <<"usecase">>, Cid, <<"uri">>], User),
   Hash = erlang:phash2(Uri),
   
   % TODO: configurable prefix
   {ok, Prefix} = application:get_env(elata_fe, datapath),
   Filename  = lists:flatten(
      io_lib:format(
         Prefix ++ '/~.16x/~s/~s/~s.png', 
         [Hash, "0x", Lid, Vid, Did]
      )
   ),
   {ok, Bin} = file:read_file(Filename),
   {ok, Bin, "image/png"}.
   
   
%%%------------------------------------------------------------------   
%%%
%%%   Private
%%%
%%%------------------------------------------------------------------    
lookup_telemetry(#user{service = Service} = User) ->
   NService = lists:map(
      fun(X) ->
         lookup_telemetry(X)
      end,
      Service
   ),
   {ok, User#user{service = NService}};

lookup_telemetry(#service{usecase = Usecase} = Service) ->
   NUsecase = lists:map(
      fun(X) ->
         lookup_telemetry(X)
      end,
      Usecase
   ),
   Service#service{usecase = NUsecase};

   
lookup_telemetry(#usecase{uri = Uri}) ->   
   case keyval_store:lookup(kv_feed, Uri) of
      {error, _} ->
         {Uri,  undefined};   
      {ok, {telemetry, _, Val}} ->
         Tele = proplists:get_value(uri, Val, []),
         Mean = proplists:get_value(mean, Tele, 0),
         {Mean}
   end.
   

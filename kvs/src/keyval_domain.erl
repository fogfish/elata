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
-module(keyval_domain).
-author(dmitry.kolesnikov@nokia.com).

%%
%% Key/Value Domain provides management interface to storage domains 
%% The interface impacts domain metadata registered within node
%% Domains entityes are not impacted
%%

-export([
   create/2,                    
   lookup/1,
   delete/1
]).

%%
%% Create definition of storage domain (domain entities are not impacted)
%%
%% Name   = string() unique domain name
%% Domain = [opt]
%%    opt = singleton | event | {name, Name} | 
%%          {plugin, Module} | {plugin, {Module, Args}} | 
%%          {key, Id}
%%    singleton = single process manages all domain entities (e.g. proxy to storage)
%%    event = storage domain generates events on CRUD operations
%%    Module = KVS plugin implementation of gen_kvs_domain / gen_kvs_entity
%%    Args = list(), list of arguments supplied to factory method
%%    Id = name of key attribute or key position
create(Name, Domain) ->
   % TODO: assert: plugin + key
   kvs_reg:register({domain, Name}, [{name, Name} | Domain]).

%%
%%
lookup(Name) ->
   kvs_reg:resolve({domain, Name}).

%%
%% Delete definition of storage domain (domain entities are not impacted)
delete(Name) ->
   kvs_reg:unregister({domain, Name}).


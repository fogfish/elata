%%

%% @depricated
%-record(agent, {
%   sys_id,      %% binary()     unique agent id  
%   dc_title,    %% binary()     human readable name
%   host,        %% {Host, Port} agent host 
%   opts=[]      %% configuration
%}).

%% @depricated
%-record(job, {
%   uri,         %% uri of use-case
%   location = []%% location to observe
%}).


%% View
-record(view, {
   sys_id,
   schema,       %% [atom()] - list of supported schemas
   scale,        %% {width, height}
   ds,           %% data streams
   theme
}).


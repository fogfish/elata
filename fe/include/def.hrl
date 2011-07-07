
%%
%% User
-record(user, {
   sys_id,          %% binary() - user id 
   email   = <<>>,  %% binary() - user's email
   service = []     %% list of services, defined by user
}).


%%
%% Service 
-record(service, {
   sys_id,          %% binary() - service id
   dc_title = <<>>, %% human readable name binary()
   usecase  = []    %% list of use-cases 
}).


%%
%% Usecase
-record(usecase, {
   sys_id,          %% binary() - usecase id
   dc_title = <<>>, %% binary() - human readable name
   uri      = <<>>  %% binary() - uri
}).



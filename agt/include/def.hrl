
% @depricated
-record(telemetry,{
   uri,   %% binary()
   val=[] %% [{key, val}]
}).

-record(uri, {
   schema   = undefined,
   host     = <<>>,
   port     = 0,
   path     = <<>>,
   q        = <<>>,
   fragment = <<>>
}).


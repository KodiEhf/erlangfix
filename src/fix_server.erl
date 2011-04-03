-module(fix_server).

-export([start_listening/0]).

-include("fix.hrl").
-include("../include/types.hrl").

start_listening() ->
    {ok, _Pid} = fix_listener:start({fix_handler, []}, {fix_parser, parse},
				    [{ip_address, '127.0.0.1'},
				     {port, 8085},
				     {also_attach, [
						    %% {handler_n, InitOptions}
						   ]}]).

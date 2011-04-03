-module(fix_listener).
-include("../include/types.hrl").
%% API
-export([start/3]).

% Start listening
-spec start(Handler::handler(), Parser::parser(),
	    Options::term()) -> {ok, Pid::pid()}.
start(Handler, Parser, Options) ->
    process_flag(trap_exit, true),
    Port = proplists:get_value(port, Options, 8085),
    IpAddress = proplists:get_value(ip_address, Options, '127.0.0.1'),
    oneshot_server:start_link(IpAddress, Port,
			      fun() ->
				      {ok, Pid} = tcp_client:start(Handler, Parser, Options),
				      Pid
			      end).

-module(gen_parsing_server).
-behaviour(gen_fsm).
-include("../include/types.hrl").
-define(TIMEOUT, infinity).
-record(state, {
	  acc = <<"">> :: binary(),
	  parser :: parser(),
	  module :: module(),
	  mod_state :: any(),
	  socket :: port()
	 }).

%% Module exports
-export([start/3]).
-export([send/2]).
-export([start_link/4]).
%% gen_fsm callbacks
-export([code_change/4,
	 handle_event/3,
	 handle_info/3,
	 handle_sync_event/4,
	 init/1,
	 terminate/3]).
%% Behaviour exports
-export([behaviour_info/1]).
%% gen_fem states
-export(['WAITING_FOR_SOCKET'/2,
	 'WAITING_FOR_MESSAGE'/2]).

%% @doc
%% Behaviour exports for initializing a "parsing tcp server"
%% @end
behaviour_info(callbacks) ->
    [{start_link, 3},
     {init, 1},
     {connected, 2},
     {message, 2},
     {disconnected, 1},
     {terminate, 2}];
behaviour_info(_) ->
    undefined.

%% @doc
%% Send data to the client.
%% @end
-spec send(Socket::socket(), Data::term()) ->
		  ok | {error, Reason::client|term()}.
send(Socket, Data) ->
    gen_tcp:send(Socket, Data),
    inet:setopts(Socket, [{active, once}]).

%% @doc
%% Start the FSM and shadowed module.
%% @end
-spec start(Module::handler(), Parser::parser(), Options::term()) ->
		   {ok, Pid::pid()}.
start(Handler, Parser, Options) ->
    start_link(?MODULE, [Handler, Parser, Options], []).

%% @doc
%% gen_parsing_server
%% @end
%%     gen_parsing_server:start_link({local, ?MODULE}, Module, Args, Options).
start_link(Name, Module, Args, Options) ->
    Port = proplists:get_value(port, Options),
    IpAddress = proplists:get_value(ip_address, Options),
    {ok, IpAddress0} = inet:getaddr(IpAddress, inet),
    Pid = spawn_link(fun() ->
			     gen_tcp:listen(Port, [binary, {packet, raw}, {reuseaddr, true},
						   {ip, IpAddress0}, {keepalive, true}, {backlog, 4096},
						   {active, false}]),
			     case Module:init(Args) of
				 {ok, ModuleState0} ->
				     Parser = proplists:get_value(parser, Options),
				     erlang:register(Name, self()),
				     gen_fsm:enter_loop(?MODULE, Options, 'WAITING_FOR_MESSAGE',
							#state{parser=Parser,module=Module,mod_state=ModuleState0}, Name);
				 {stop, Reason, State} ->
				     Module:terminate(Reason, State),
				     exit(self())
			     end
				 
		     end),
    {ok, Pid}.
start_link(Module, Args, Options) ->
    Port = proplists:get_value(port, Options),
    IpAddress = proplists:get_value(ip_address, Options),
    {ok, IpAddress0} = inet:getaddr(IpAddress, inet),
    Pid = spawn_link(fun() ->
			     gen_tcp:listen(Port, [binary, {packet, raw}, {reuseaddr, true},
						   {ip, IpAddress0}, {keepalive, true}, {backlog, 4096},
						   {active, false}]),
			     case Module:init(Args) of
				 {ok, ModuleState0} ->
				     Parser = proplists:get_value(parser, Options),
				     gen_fsm:enter_loop(?MODULE, Options, 'WAITING_FOR_MESSAGE', #state{parser=Parser,
													module=Module,
													mod_state=ModuleState0});
				 {stop, Reason, State} ->
				     Module:terminate(Reason, State),
				     exit(self())
			     end
			     
		     end),
    {ok, Pid}.

%% Gen_fsm callbacks
init([Module, Args, Options]) ->
    case Module:init(Args) of
	{ok, State} ->
	    Parser = proplists:get_value(parser, Options),
	    {ok, 'WAITING_FOR_SOCKET', #state{parser=Parser, module=Module,
					      mod_state=State}, ?TIMEOUT};
	{stop, Reason, State} ->
	    {stop, Reason, #state{ mod_state = State }}
    end.

%% @doc
%% A socket has arrived
%% @end.
'WAITING_FOR_SOCKET'({socket_ready, Socket}, #state{module = Module,
						    mod_state = ModuleState} = State) ->
    case Module:connected(Socket, ModuleState) of
	{ok, ModuleState0} ->
	    {next_state, 'WAITING_FOR_MESSAGE', State#state{socket=Socket,
							    mod_state=ModuleState0}, ?TIMEOUT};
	{stop, Reason, ModuleState0} ->
	    {stop, Reason, #state{ mod_state = ModuleState0 }}
    end.

%% @doc
%% Messages has arrived
%% @end
'WAITING_FOR_MESSAGE'({data, Data}, #state{acc=Acc, parser=Parser, module=Module,
					   mod_state=ModuleState} = State) ->
    Data0 = <<Data/binary, Acc/binary>>,
    case Parser:parse(Data0) of
	{ok, Message, Rest} ->
	    case Module:message(Message, ModuleState) of
		{ok, ModuleState0} ->
		    {next_state, 'WAITING_FOR_MESSAGE', State#state{acc=Rest, mod_state=ModuleState0}, ?TIMEOUT};
		{stop, Reason, ModuleState0} ->
		    {stop, Reason, State#state{mod_state=ModuleState0, acc=Rest}}
	    end;   
	{incomplete, Rest} ->
	    {next_state, 'WAITING_FOR_MESSAGE', State#state{acc=Rest}, ?TIMEOUT};
	{invalid, Rest} ->
	    case Module:message(invalid, ModuleState) of
		{ok, ModuleState0} ->
		    {next_state, 'WAITING_FOR_MESSAGE', State#state{mod_state=ModuleState0}, ?TIMEOUT};
		{stop, Reason, ModuleState0} ->
		    {stop, Reason, State#state{mod_state=ModuleState0, acc=Rest}}
	    end
    end.

handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData, ?TIMEOUT}.

handle_sync_event(_Event, _From, StateName, StateData) ->
    Reply = ok,
    {reply, Reply, StateName, StateData, ?TIMEOUT}.

%% @doc
%% Incoming socket!
%% @end
handle_info({socket_ready, Socket} = Incoming, StateName, State) when is_port(Socket) ->
    inet:setopts(Socket, [{active, once}]),
    StateName(Incoming, State);

%% @doc
%% Incoming data!
%% @end
handle_info({tcp, Socket, Data}, StateName, State) when is_port(Socket) ->
    StateName({data, Data}, State);

handle_info(Info, StateName, #state{module=Module,mod_state=ModuleState} = State) ->
    case Module:handle_info(Info, ModuleState) of
	{noreply, ModuleState0} ->
	    {next_state, StateName, State#state{mod_state=ModuleState0}, ?TIMEOUT};
	{stop, Reason, ModuleState0} ->
	    {stop, Reason, State#state{mod_state=ModuleState0}}
    end.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%% @doc
%% Terminating the FSM and the users event manager. Since I'm using
%% gen_event's stop method, I don't need to announce the handlers 
%% as this will send a stop message to every attached handler.
%% @end
-spec terminate(Reason::atom, StateName::atom(), State::#state{}) ->
		       any().
terminate(_Reason, _StateName, #state{}) ->
    ok.

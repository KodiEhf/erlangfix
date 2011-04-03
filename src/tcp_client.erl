-module(tcp_client).
-behaviour(gen_fsm).

-include("../include/types.hrl").

%% Module exports
-export([start/3]).
-export([send/2]).
%% gen_fsm callbacks
-export([code_change/4,
	 handle_event/3,
	 handle_info/3,
	 handle_sync_event/4,
	 init/1,
	 terminate/3]).
%% gen_fem states
-export(['WAITING_FOR_SOCKET'/2,
	 'WAITING_FOR_MESSAGE'/2]).

-define(TIMEOUT, infinity).

-record(state, {
	  user_event_mgr :: pid(),
	  acc = <<"">> :: binary(),
	  parser :: parser(),
	  socket :: port()
	 }).

%% @doc
%% Send data to the client.
%% @end
-spec send(Client::socket(), Data::term()) ->
		  ok | {error, Reason::client|term()}.
send(Client, Data) ->
    gen_tcp:send(Client, Data).

%% @doc
%% Start the FSM. This will also create a event manager just for this user,
%% which can be used to communicate with him.
%% @end
-spec start(Handler::handler(), Parser::parser(), Options::term()) ->
		   {ok, Pid::pid()}.
start(Handler, Parser, Options) ->
    start_link(?MODULE, [Handler, Parser, Options], []).

-spec start_link(Module::module, list(), []) ->
			{ok, Pid::pid()}.
start_link(Module, [Handler, Parser, Options], []) ->
    {ok, Pid} = gen_fsm:start_link(Module, [Handler, Parser, Options], []),
    {ok, Pid}.

%% Gen_fsm callbacks
init([{HandlerMod, HandlerArgs}, Parser, Options]) ->
    MgrPid = gen_event:start_link(),
    ok = gen_event:add_sup_handler(MgrPid, HandlerMod, [HandlerArgs | {manager, MgrPid}]),
    lists:foreach(fun({HandlerMod0, HandlerArgs0}) ->
			  gen_event:add_sup_handler(MgrPid, HandlerMod0, [HandlerArgs0 |
										  {manager, MgrPid}])
			  end, proplists:get_value(also_attach, Options, [])),
    {ok, 'WAITING_FOR_SOCKET', #state{user_event_mgr=MgrPid, parser=Parser}, ?TIMEOUT}.

%% @doc
%% A socket has arrived
%% @end.
'WAITING_FOR_SOCKET'({client_connected, Socket}, #state{user_event_mgr=MgrPid} = State) ->
    gen_event:notify(MgrPid, {connected, {client, Socket}}),
    {next_state, 'WAITING_FOR_MESSAGE', State#state{socket=Socket}, ?TIMEOUT}.

%% @doc
%% Messages has arrived
%% @end
'WAITING_FOR_MESSAGE'({data, Data}, #state{user_event_mgr = MgrPid, acc = Acc,
					       parser = {Parser, Method}} = State) ->
    Data0 = <<Acc/binary, Data/binary>>,
    case Parser:Method(Data0) of
	{ok, Message, Rest} ->
	    gen_event:notify(MgrPid, {message, Message}),
	    {next_state, 'WAITING_FOR_MESSAGE', State#state{acc=Rest}, ?TIMEOUT};
	{incomplete, Rest} ->
	    {next_state, 'WAITING_FOR_MESSAGE', State#state{acc=Rest}, ?TIMEOUT};
	invalid ->
	    gen_event:notify(MgrPid, {client, gone}),
	    {stop, invalid_message, State}
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
    % Flow control: enable forwarding of next TCP message
    inet:setopts(Socket, [{active, once}]),
    fix_client_fsm:StateName(Incoming, State);

%% @doc
%% Incoming data!
%% @end
handle_info({tcp, Socket, Data}, StateName, State) when is_port(Socket) ->
    fix_client_fsm:StateName({data, Data}, State);

handle_info(_Info, StateName, StateData) ->
    {next_state, StateName, StateData, ?TIMEOUT}.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%% @doc
%% Terminating the FSM and the users event manager. Since I'm using
%% gen_event's stop method, I don't need to announce the handlers 
%% as this will send a stop message to every attached handler.
%% @end
-spec terminate(Reason::atom, StateName::atom(), State::#state{}) ->
		       any().
terminate(_Reason, _StateName, #state{user_event_mgr = MgrPid }) ->
    gen_event:stop(MgrPid).

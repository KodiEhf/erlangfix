-module(fix_server).
-behaviour(gen_parsing_server).

%% Behaviour exports
-export([start_link/3,
	 init/1,
	 connected/2,
	 disconnected/1,
	 message/2,
	 terminate/2,
	 handle_info/2]).

-export([start/0]).

-include("fix.hrl").
-include("../include/types.hrl").
-record(state, {
	  client::term()
	 }).

start() ->
    Options = [{parser, parse},
	       {ip_address, '127.0.0.1'},
	       {port, 8085}],
    start_link(?MODULE, [], Options).

%% @doc
%% Start the server
%% @end
%% @todo spec up the options proplist
-spec start_link(Module::module(), Args::list(), Options::[term()]) ->
			{ok, Pid::pid()}.
start_link(Module, Args, Options) ->
    gen_parsing_server:start_link({local, ?MODULE}, Module, Args, Options).

%% @doc
%% Initialize the server
%% @end
-spec init(Args::list()) ->
		  {ok, State::#state{}} |
		  {stop, Reason::any(), State::#state{}}.
init([]) ->
    {ok, #state{}}.

%% @doc
%% A user has connected
%% @end
-spec connected(Client::term(), State::#state{}) ->
		       {ok, State::#state{}} |
		       {stop, Reason::any(), State::#state{}}.
connected(Client, State) ->
    {ok, State#state{client = Client}}.

%% @doc
%% A user has left
%% @end
-spec disconnected(State::#state{}) ->
			  {ok, State::#state{}} |
			  {stop, Reason::any(), State::#state{}}.
disconnected(State) ->
    {stop, normal, State}.

%% @doc
%% Valid message has been recieved
%% @end
-spec message(Message::any(), State::#state{}) ->
		     {ok, State::#state{}} |
		     {stop, Reason::any(), State::#state{}}.
message(Message, State) ->
    io:format("Got message ~p~n", [Message]),
    {ok, State}.

%% @doc
%% Other messages sent to the pid
%% @end
handle_info(_Message, State) ->
    {noreply, State}.

%% @doc
%% Server is terminating
%% @end
-spec terminate(Reason::any(), State::#state{}) ->
		       ok.
terminate(_Reason, _State) ->
    ok.

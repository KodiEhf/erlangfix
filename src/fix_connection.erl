%%%-------------------------------------------------------------------
%%% @author Dagur Gunnarsson <dagur@snake.vodafone>
%%% @copyright (C) 2011, Dagur Gunnarsson
%%% @doc
%%%
%%% @end
%%% Created : 20 Mar 2011 by Dagur Gunnarsson <dagur@snake.vodafone>
%%%-------------------------------------------------------------------
-module(fix_connection).
-behaviour(gen_server).

-include("fix.hrl").

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, send/1]).

-define(SERVER, ?MODULE). 

-record(state, {lsocket, socket, port, seq_no=1}).

-define(PORT, 2222).
-define(TIMEOUT, 60000). %one minute

%%%===================================================================
%%% API
%%%===================================================================
start_link(Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

send(Message) ->
    gen_server:cast(?SERVER, {send, Message}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([Port]) ->
    {ok, ListeningSocket} = gen_tcp:listen(Port, [binary, {packet, 0}, {active, false}]),
    gen_server:cast(?SERVER, accept),
    {ok, #state{lsocket=ListeningSocket, port=port}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(accept, State = #state{lsocket=ListeningSocket}) ->
    Result = gen_tcp:accept(ListeningSocket, 1000),
    case Result of
	{ok, Socket} -> gen_server:cast(?SERVER, receive_loop), 
			{noreply, State#state{socket=Socket}};
	{error, timeout} -> gen_server:cast(?SERVER, accept),
			    {noreply, State};
	{error, closed} -> io:format("closed ~n"),
			   {stop, closed, State}
    end;    

handle_cast(receive_loop, State = #state{socket=Socket}) ->
    case gen_tcp:recv(Socket, 0) of
	{ok, Data} ->
	    io:format("Data is ~p~n", [Data]),
	    gen_server:cast(?SERVER, receive_loop);
	{error, closed} ->  io:format("error connection closed ~n");
	{error, timeout} -> io:format("error closed ~n")
    end,
    {noreply, State};

handle_cast({send, Message}, State = #state{socket=Socket}) ->
    gen_tcp:send(Socket, Message),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

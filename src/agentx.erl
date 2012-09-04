%%%----------------------------------------------------------------------
%%% File    : agentx.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : host and node monitor agent.
%%% Created : 27 Dec 2010
%%% License : http://www.opengoss.com
%%%
%%% Copyright (C) 2010, www.opengoss.com 
%%%----------------------------------------------------------------------
-module(agentx).

-author('ery.lee@gmail.com').

-include_lib("elog/include/elog.hrl").

-import(extbif, [appvsn/0]).

-import(erlang, [send_after/3]).

-export([start_link/0,
		stop/0]).

-behavior(gen_server).

-export([init/1, 
        handle_call/3, 
        handle_cast/2, 
        handle_info/2, 
        terminate/2, 
        code_change/3 ]).

-record(state, {channel}).
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    {ok, Conn} = amqp:connect(),
    Channel = open(Conn),
    erlang:send_after(60 * 1000, self(), check_host),
    ?INFO("Agent is started...[ok]", []),
    {ok, #state{channel = Channel}}.

open(Conn) ->
	{ok, Channel} = amqp:open_channel(Conn),
	amqp:topic(Channel, <<"sys.watch">>),
	{ok, Q} = amqp:queue(Channel, node()),
	amqp:bind(Channel, <<"sys.watch">>, Q, <<"ping">>),
	amqp:consume(Channel, Q),
	%send presence
	Presence = {node(), node, available, extlib:appvsn(), <<"startup">>},
	amqp:send(Channel, <<"presence">>, term_to_binary(Presence)),
	erlang:send_after(20000, self(), heartbeat),
	Channel.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(Req, _From, State) ->
    ?ERROR("badreq: ~p", [Req]),
    {reply, {badreq, Req}, State}.

handle_cast(Msg, State) ->
    ?ERROR("badmsg: ~p", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}, 
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(check_host,#state{channel =  Channel} = State) ->
    WorkerNum = length(nodes() -- [node()]),
    Nodes = string:join([atom_to_list(N) || N <- nodes()], ","),
    {ok, HostName} = inet:gethostname(),
    HostDn = list_to_binary(["host=", HostName]),
    try 
        {unix, OsType} = os:type(),
        case OsType of
        'hp-ux' ->
            mon_hpux:run([{dn, HostDn}]);
        'aix' ->
            mon_aix:run([{dn, HostDn}]);
        _ ->
            mon_unix:run([{dn, HostDn}]) 
        end
    of
    {ok, HostInfo, Metrics} ->
        HostInfo1 = [{dn, HostDn},
					 {name, HostName},
					 {presence, 1},
					 {jid, node()},
					 {worker_num, WorkerNum},
					 {workers, Nodes} | HostInfo],
	case Channel ==  undefined of
	false ->
	Payload = term_to_binary({host, HostInfo1}, [compressed]),
        amqp:send(Channel, <<"metric">>, term_to_binary(Metrics)),
        amqp:send(Channel, <<"host">>, Payload);
	true ->
		ignore
	end
    catch
    _:Ex ->
		{error, Ex}
    end,
    erlang:send_after(300*1000, self(), check_host),
    {noreply, State};

handle_info(heartbeat, #state{channel=undefined} = State) ->
    {noreply, State};

handle_info(heartbeat, #state{channel= Channel} = State) ->
    Heartbeat = {node(), <<"agent is alive.">>, []},
    amqp:send(Channel, <<"heartbeat">>, term_to_binary(Heartbeat)),
    send_after(20000, self(), heartbeat),
    {noreply, State};

handle_info(Info, State) ->
    ?ERROR("badinfo: ~p", [Info]),
    {noreply, State}.


terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


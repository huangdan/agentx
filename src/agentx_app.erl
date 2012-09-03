%%%----------------------------------------------------------------------
%%% File    : agentx_app.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : agent application
%%% Created : 13 Jan 2010
%%% License : http://www.opengoss.com
%%%
%%% Copyright (C) 2012, www.opengoss.com
%%%----------------------------------------------------------------------
-module(agentx_app).

-author('ery.lee@gmail.com').

-behavior(application).

-export([start/2, stop/1]).

-export([start/0, stop/0]).
    
start() ->
    [application:start(App) || App <- [sasl, crypto, elog, evmon]],
    {_, OsType} = os:type(),
	start_osmon(OsType).

stop() ->
    {_, OsType} = os:type(),
    [stop_app(App) || App <- lists:reverse(apps(OsType))],
    [application:stop(App) || App <- [evmon, elog, crypto, sasl]].

start(normal, _Args) ->
	agentx_sup:start_link().

stop(_) ->
	ok.

start_osmon('hp-ux') ->
    ignore;
start_osmon(aix) ->
    ignore;
start_osmon(_) ->
    application:start(os_mon).
	
apps('hp-ux') ->
    [sasl, agent];
apps(aix) ->
    [sasl, agent];
apps(_) ->
    [sasl, os_mon, agent].

stop_app(App) ->
    application:stop(App).



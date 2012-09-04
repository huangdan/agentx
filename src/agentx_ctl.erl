%%%----------------------------------------------------------------------
%%% File    : agent_ctl.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : Opengoss node control
%%% Created : 15 Jan 2010
%%% License : http://www.opengoss.com
%%%
%%% Copyright (C) 2010, www.opengoss.com
%%%----------------------------------------------------------------------
-module(agentx_ctl).

-author('ery.lee@gmail.com').

-compile(export_all).

-include_lib("elog/include/elog.hrl").

-define(STATUS_SUCCESS, 0).

-define(STATUS_ERROR,   1).

-define(STATUS_USAGE,   2).

-define(STATUS_BADRPC,  3).

start() ->
    case init:get_plain_arguments() of
	[SNode | Args] ->
	    Node = node_name(SNode),
	    Status = case rpc:call(Node, ?MODULE, process, [Args]) of
			 {badrpc, Reason} ->
			     ?PRINT("RPC failed on the node ~p: ~p~n",
				       [Node, Reason]),
			     ?STATUS_BADRPC;
			 S ->
			     S
		     end,
	    halt(Status);
	_ ->
	    print_usage(),
	    halt(?STATUS_USAGE)
    end.

init() ->
	ok.

node_name(SNode) ->
    SNode1 = 
    case string:tokens(SNode, "@") of
    [_Node, _Server] ->
        SNode;
    _ ->
        case net_kernel:longnames() of
         true ->
             SNode ++ "@" ++ inet_db:gethostname() ++
                  "." ++ inet_db:res_option(domain);
         false ->
             SNode ++ "@" ++ inet_db:gethostname();
         _ ->
             SNode
         end
    end,
    list_to_atom(SNode1).

status() ->
    {InternalStatus, ProvidedStatus} = init:get_status(),
    ?PRINT("Node ~p is ~p. Status: ~p~n",
              [node(), InternalStatus, ProvidedStatus]),
    case lists:keysearch(agentx, 1, application:which_applications()) of
        false ->
            ?PRINT("agentx is not running~n", []),
            ?STATUS_ERROR;
        {value,_Version} ->
            ?PRINT("agentx is running~n", []),
            ?STATUS_SUCCESS
    end.

print_usage() ->
	CmdDescs = [{"status", "get node status"},
	 {"stop", "stop node"},
	 {"restart", "restart node"},
	 {"log_rotation", "log rotation"},
	 {"mnesia [info]", "show information of Mnesia system"}],
    MaxCmdLen =
	lists:max(lists:map(
		    fun({Cmd, _Desc}) ->
			    length(Cmd)
		    end, CmdDescs)),
    NewLine = io_lib:format("~n", []),
    FmtCmdDescs =
	lists:map(
	  fun({Cmd, Desc}) ->
		  ["  ", Cmd, string:chars($\s, MaxCmdLen - length(Cmd) + 2),
		   Desc, NewLine]
	  end, CmdDescs),
    ?PRINT(
      "Usage: node_ctl [--node nodename] command [options]~n"
      "~n"
      "Available commands in this node:~n"
      ++ FmtCmdDescs ++
      "~n"
      "Examples:~n"
      "  node_ctl restart~n"
      "  node_ctl --node node@host restart~n",
     []).



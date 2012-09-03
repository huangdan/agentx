-module(agentx_sup).

-author("ery.lee@gmail.com").

-behavior(supervisor).

-export([start_link/0, init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Agent = {agentx, {agentx, start_link, []},
		permanent, 5000, worker, [agent]},

	{ok, {{one_for_one, 10, 100}, [Agent]}}.


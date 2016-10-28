%%%-------------------------------------------------------------------
%% @doc httpd top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(httpd_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    Children = [
                {"httpd_tcp_listener", {httpd_tcp_listener, start_link, []}, permanent, 5, worker, []},
                {"httpd_tcp_worker_sup", {httpd_tcp_worker_sup, start_link, []}, permanent, 5, supervisor, []}
               ],
    {ok, { {one_for_all, 0, 1}, Children} }.

%%====================================================================
%% Internal functions
%%====================================================================

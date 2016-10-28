-module(httpd_tcp_worker_sup).

-export([start_link/0, start_child/1, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Socket) ->
    supervisor:start_child(?MODULE, [Socket]).

init([]) ->
    Children = [
                {"httpd_tcp_worker", {httpd_tcp_worker, start_link, []}, temporary, 5, worker, []}
               ],
    {ok, {{simple_one_for_one, 0, 1}, Children}}.

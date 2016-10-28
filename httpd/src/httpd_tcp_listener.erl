-module(httpd_tcp_listener).

-export([start_link/0]).

start_link() ->
    Pid = spawn_link(fun init/0),
    {ok, Pid}.

init() ->
    Port = 8888,
    Backlog = 10244,
    Options = [binary,
              inet6, % support both ipv4 and ipv6
              {active, false},
              {reuseaddr, true},
              {backlog, Backlog}
             ],
    {ok, Listen} = gen_tcp:listen(Port, Options),
    lists:foreach(fun(_) -> spawn(fun() -> accept(Listen) end) end,  lists:seq(1, 100)),
    accept(Listen).

accept(Listen) ->
    {ok, Ref} = prim_inet:async_accept(Listen, -1),
	receive
		{inet_async, Listen, Ref, {ok, Socket}} ->
			inet_db:register_socket(Socket, inet6_tcp),
			process(Socket, 0, 10 * 1000),
			accept(Listen);
		{inet_async, Listen, Ref,{error,closed}} ->
			ok;
		X -> 
            io:format("fail accept ~p~n", [X])
	end.

process(Socket, Size, Timeout) ->
    case gen_tcp:recv(Socket, Size, Timeout) of
        {ok, Packet} ->
            Response = response(Packet),
            gen_tcp:send(Socket, Response);
        {error, closed} ->
			ok;
        {error, Reason} ->
            io:format("fail recv ~p~n", [Reason])
    end,
	catch erlang:port_close(Socket).

response(_Request) ->
    <<"HTTP/1.0 200 OK\r\nDate: Tue, 25 Oct 2016 10:21:33 GMT\r\nConnection: close\r\nContent-Type: text/plain; charset=utf-8\r\nContent-Length: 11\r\n\r\nhello world">>.

-module(httpd_tcp_worker).

-export([start_link/1]).

start_link(Socket)->
    Pid = spawn_link(fun() -> process(Socket, 0, 30 * 1000) end),
    {ok, Pid}.

process(Socket, Size, Timeout) ->
    case gen_tcp:recv(Socket, Size, Timeout) of
        {ok, Packet} ->
            Response = response(Packet),
            gen_tcp:send(Socket, Response),
            gen_tcp:close(Socket);
        {error, closed} ->
            gen_tcp:close(Socket);
        {error, Reason} ->
            io:format("fail recv ~p~n", [Reason]),
            gen_tcp:close(Socket)
    end.

response(_Request) ->
    <<"HTTP/1.0 200 OK\r\nDate: Tue, 25 Oct 2016 10:21:33 GMT\r\nConnection: close\r\nContent-Type: text/plain; charset=utf-8\r\nContent-Length: 11\r\n\r\nhello world">>.

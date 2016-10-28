-module(httpd_profile).

-export([run/0]).

run() ->
    eprof:start_profiling(processes()),
    httpd_sup:start_link(),

    timer:sleep(10 * 1000), % ここで待っている間にwrkで負荷をかけます

    eprof:stop_profiling(),
    eprof:analyze(total).

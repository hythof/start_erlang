今からでも間に合うErlang入門

Ubuntu 16.04でErlangのインストールから簡単なサンプルアプリケーションの作成まで行ってみます。

## Erlang開発環境構築（初期設定）

### 依存ファイルをインストール

```shell-session
sudo apt-get install build-essential libncurses5-dev openssl libssl-dev
sudo apt-get install curl git-core
```

### Erlangのバージョン管理、Kerlをインストール

```shell-session
mkdir -p ~/local/bin
cd ~/local/bin
curl -O https://raw.githubusercontent.com/kerl/kerl/master/kerl
chmod a+x kerl
```

### kerlでErlang 19.1をインストール

```shell-session
./kerl list releases
./kerl install 19.1 19.1
./kerl build 19.1 19.1
./kerl install 19.1 ~/erlang/19.1
```

インストールした Erlang 19.1 を有効化します。

```shell-session
. ~/erlang/19.1/activate
```

whichでみるとerlコマンドが使えるようになり、Erlangの開発環境が整いました。
erlコマンドを実行するとErlangのshellが起動し、プログラムの動作確認などがインタラクティブに行なえます。

```shell-session
which erl
# /home/username/erlang/19.1/bin/erl
```

## 簡単なネットワークアプリケーションの開発
Erlangで簡単なhttpdサーバを作ってみます。
またOTP

### ビルドツールのrebarを使う
rebarを使うとコンパイル、テスト、配布用のtar.gzの作成などがコマンド一つでできるので便利です。
rebarは最新のversion 3を使います。version 2はDeprecatedなので注意。

```shell-session
mkdir project
cd project
wget https://s3.amazonaws.com/rebar3/rebar3
chmod 0755 rebar3
```

### rebarで定形ファイル作成

```shell-session
./rebar3 new app httpd
# ===> Writing httpd/src/httpd_app.erl
# ===> Writing httpd/src/httpd_sup.erl
# ===> Writing httpd/src/httpd.app.src
# ===> Writing httpd/rebar.config
# ===> Writing httpd/.gitignore
# ===> Writing httpd/LICENSE
# ===> Writing httpd/README.md
```

### 実装
まず簡易的なhttpd実装してみます。
処理の流れは以下のイメージです。

1. httpd_tcp_listener が新規接続してきたHTTPクライアントを受付
2. httpd_tcp_worker_sup の start_child() で1つのTCPストリームを処理するプロセスを起動（Erlangのプロセスはマイクロスレッドを意味します）
3. 起動したプロセスは httpd_tcp_worker の start_link() でTCPストリームからHTTPリクエストを読み込み、HTTPレスポンスを書き込む

```erlang:src/httpd_tcp_listener.erl
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
    accept(Listen).

accept(Listen) ->
    case gen_tcp:accept(Listen) of
        {ok, Socket} ->
            {ok, Pid} = httpd_tcp_worker_sup:start_child(Socket),
            gen_tcp:controlling_process(Socket, Pid);
        {error, Reason} ->
            io:format("fail accept ~p~n", [Reason])
    end,
    accept(Listen).
```

```erlang:src/httpd_tcp_worker.erl
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
        {error, Reason} ->
            io:format("fail recv ~p~n", [Reason]),
            gen_tcp:close(Socket)
    end.

response(_Request) ->
    <<"HTTP/1.0 200 OK\r\nDate: Tue, 25 Oct 2016 10:21:33 GMT\r\nConnection: close\r\nContent-Type: text/plain; charset=utf-8\r\nContent-Length: 11\r\n\r\nhello world">>.
```

```erlang:src/httpd_tcp_worker_sup.erl
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
```

また httpd_sup.erl の init([]) を編集し、追加実装した部分の起動処理を加えます。

```erlang:src/httpd_sup.erl
%% ...(省略)...
init([]) ->
    Children = [
                {"httpd_tcp_listener", {httpd_tcp_listener, start_link, []}, permanent, 5, worker, []},
                {"httpd_tcp_worker_sup", {httpd_tcp_worker_sup, start_link, []}, permanent, 5, supervisor, []}
               ],
    {ok, { {one_for_all, 0, 1}, Children} }.
```


## 動作確認

アプリケーションの実装が終わったので起動してみます。

```shell-session
../rebar3 compile && erl -pa _build/default/lib/*/ebin -eval 'application:start(httpd).' -noshell
```

ブラウザからアクセスするとhello worldと表示されました。
![hello_world.png](https://qiita-image-store.s3.amazonaws.com/0/39587/986a234b-d268-b499-6806-0aedc98279c6.png)

せっかくなのでベンチマークも取ってみます。
wrkで計測したところ、このプログラムは15,380/秒に対してnginxが90,107/秒でした。nginxの方が5.8倍程早いようです。


```shell-session:wrkインストール
sudo apt-get install wrk
```

```shell-session:erlang ベンチマーク
wrk -c 100 -d 10 -t 10 http://localhost/
#Running 10s test @ http://localhost/
#  10 threads and 100 connections
#  Thread Stats   Avg      Stdev     Max   +/- Stdev
#    Latency     7.32ms   16.20ms 261.95ms   91.41%
#    Req/Sec     9.09k     2.86k   29.55k    75.05%
#  910073 requests in 10.10s, 213.46MB read
#Requests/sec:  90107.61
#Transfer/sec:     21.14MB
```

```shell-session:nginx ベンチマーク
wrk -c 100 -d 10 -t 10 http://localhost:8888/
#Running 10s test @ http://localhost:8888/
#  10 threads and 100 connections
#  Thread Stats   Avg      Stdev     Max   +/- Stdev
#    Latency     6.43ms  815.17us  21.68ms   84.57%
#    Req/Sec     1.56k   184.27     5.17k    98.60%
#  155320 requests in 10.10s, 21.77MB read
#Requests/sec:  15380.09
#Transfer/sec:      2.16MB
```

悔しいのでこのプログラムを高速化するためプロファイリングしてみます。


## プロファイリングの仕方

profile用のファイルを追加します。

```erlang:src/httpd_profile.erl
-module(httpd_profile).                                                                   

-export([run/0]).

run() ->
    eprof:start_profiling(processes()),
    httpd_sup:start_link(),

    timer:sleep(10 * 1000), % ここで待っている間にwrkで負荷をかけます
    
    eprof:stop_profiling(),
    eprof:analyze(total).
```

サーバを起動します。

```shell-session
../rebar3 compile && erl -pa _build/default/lib/*/ebin -eval 'httpd_profile:run(),init:stop().' -noshell 
```

別のshellでwrkで負荷をかけます。

```shell-session
wrk -c 100 -d 10 -t 1 http://localhost:8888/
```

```text:Profile 結果の抜粋
FUNCTION                                       CALLS        %   TIME  [uS / CALLS]
--------                                       -----  -------   ----  [----------]
httpd_tcp_worker:process/3                       253     0.03     11  [      0.04]
httpd_tcp_worker:response/1                      252     0.06     23  [      0.09]
httpd_tcp_worker:'-start_link/1-fun-0-'/1        253     0.06     24  [      0.09]
httpd_tcp_listener:accept/1                      254     0.15     59  [      0.23]
httpd_tcp_worker:start_link/1                    253     0.21     83  [      0.33]
httpd_tcp_worker_sup:start_child/1               254     4.99   2010  [      7.91]
```

start_child() がネックになっているのでこれを使わない形に書き換えます。

```erlang:src/httpd_tcp_listener.erl
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
    case gen_tcp:accept(Listen) of
        {ok, Socket} ->
            %{ok, Pid} = httpd_tcp_worker_sup:start_child(Socket),
            %gen_tcp:controlling_process(Socket, Pid),
            process(Socket, 0, 10 * 1000),
            accept(Listen);
        {error, closed} ->
            ok;
        {error, Reason} ->
            io:format("fail accept ~p~n", [Reason])
    end.

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
```

この状態でベンチマークを取ってみます。

```shell-session: 複数 accept ベンチマーク
wrk -c 100 -d 10 -t 10 http://localhost:8888/
Running 10s test @ http://localhost:8888/
  10 threads and 100 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     2.22ms    1.69ms  39.34ms   82.43%
    Req/Sec     4.76k   471.03     8.46k    77.71%
  475838 requests in 10.09s, 66.71MB read
Requests/sec:  47155.42
Transfer/sec:      6.61MB
```

15,380/秒から47,155/秒と3倍近く早くなりました。
またプロファイルしてみます。

```text:Profile 結果の抜粋
FUNCTION                                          CALLS        %      TIME  [uS / CALLS]
--------                                          -----  -------      ----  [----------]
httpd_tcp_listener:accept/1                      143313     0.17     32754  [      0.23]
httpd_tcp_listener:response/1                    143191     0.30     57481  [      0.40]
httpd_tcp_listener:process/3                     143212     0.33     62697  [      0.44]
prim_inet:type_opt/2                            3293861     5.41   1020978  [      0.31]
erts_internal:port_close/1                       143228     8.21   1549668  [     10.82]
erts_internal:port_control/3                    1145802    12.69   2396415  [      2.09]
erts_internal:port_command/3                     143207    17.96   3390508  [     23.68]
```

アプリ部分とに時間がかかってそうな部分とボトルネック部分を抜粋しました。
erts_internal:port_... がボトルネックなのでこれをどうにかしてみます。

```erlang:src/httpd_tcp_listener.erl 
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
```

再度ベンチマークを取ります。

```shell-session: acync_accept() のベンチマーク結果
wrk -c 100 -d 10 -t 10 http://localhost:8888/
# Running 10s test @ http://localhost:8888/
#   10 threads and 100 connections
#   Thread Stats   Avg      Stdev     Max   +/- Stdev
#     Latency     1.76ms  447.03us  23.40ms   89.45%
#     Req/Sec     5.63k   483.33    11.85k    88.94%
#   562511 requests in 10.10s, 78.86MB read
# Requests/sec:  55704.12
# Transfer/sec:      7.81MB
```

47,155/秒から55704/秒と少し早くなりました。
gen_tcp:close() からも erts_internal:port_control() が呼ばれているので、代わりに erlang:port_close() を直接使ってみます。

```erlang:src/httpd_tcp_listener.erl
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
```

またベンチマークします。

```shell-session: port_close のベンチマーク結果
wrk -c 100 -d 10 -t 10 http://localhost:8888/
# Running 10s test @ http://localhost:8888/
#   10 threads and 100 connections
#   Thread Stats   Avg      Stdev     Max   +/- Stdev
#     Latency     1.63ms    1.23ms  26.66ms   88.33%
#     Req/Sec     6.51k   766.23    18.27k    88.05%
#   650089 requests in 10.10s, 91.14MB read
# Requests/sec:  64369.12
# Transfer/sec:      9.02MB
```

55704/秒から64,369/秒とさらに少し早くなりました。
この辺りで最適化は止めてベンチマーク結果をまとめてみます。

| ベンチマーク対象                                     | Request/sec   |
|------------------------------------------------------|---------------|
| nginx                                                | 90,107        |
| accept毎にspawn                                      | 15,380        |
| 複数accept                                           | 47,155        |
| gen_tcp:accept() の代わりに prim_inet:acync_accept() | 55,704        |
| gen_tcp:close() の代わりに erlang:port_close         | 64,369        |


## まとめ
Erlang言語を使えばネットワークサーバが比較的簡単に書けるようです。
またプロファイリングと改善を繰り返せばC言語で実装されたnginxに迫る性能がでることも確認できました。

最後のプログラムでHTTPリクエストを100回実行したプロファイル結果を載せておきます。
```text:Profile 結果
FUNCTION                                       CALLS        %  TIME  [uS / CALLS]
--------                                       -----  -------  ----  [----------]
gen:start/6                                        2     0.00     0  [      0.00]
gen:do_spawn/6                                     2     0.00     0  [      0.00]
gen:init_it/7                                      2     0.00     0  [      0.00]
gen:init_it2/7                                     2     0.00     0  [      0.00]
gen:call/4                                         1     0.00     0  [      0.00]
gen:do_call/4                                      1     0.00     0  [      0.00]
gen:do_for_proc/2                                  1     0.00     0  [      0.00]
gen:where/1                                        2     0.00     0  [      0.00]
gen:register_name/1                                2     0.00     0  [      0.00]
gen:name/1                                         2     0.00     0  [      0.00]
gen:spawn_opts/1                                   2     0.00     0  [      0.00]
gen:debug_options/2                                2     0.00     0  [      0.00]
gen:'-call/4-fun-0-'/4                             1     0.00     0  [      0.00]
application:get_env/2                              1     0.00     0  [      0.00]
prim_file:get_cwd/0                                3     0.00     0  [      0.00]
prim_file:get_cwd_int/1                            3     0.00     0  [      0.00]
prim_file:handle_fname_response/1                  3     0.00     0  [      0.00]
prim_file:'-get_cwd_int/2-fun-0-'/1                3     0.00     0  [      0.00]
lists:reverse/1                                    7     0.00     0  [      0.00]
lists:seq/2                                        1     0.00     0  [      0.00]
lists:keydelete/3                                  3     0.00     0  [      0.00]
code_server:call/1                                 6     0.00     0  [      0.00]
code_server:try_load_module_2/6                    6     0.00     0  [      0.00]
code_server:load_file_1/3                          6     0.00     0  [      0.00]
code_server:absname/1                              3     0.00     0  [      0.00]
code_server:absname/2                              3     0.00     0  [      0.00]
code_server:'-ensure_loaded/3-fun-0-'/4            6     0.00     0  [      0.00]
httpd_sup:start_link/0                             1     0.00     0  [      0.00]
httpd_sup:init/1                                   1     0.00     0  [      0.00]
filename:absname_join/2                            3     0.00     0  [      0.00]
filename:join/2                                    3     0.00     0  [      0.00]
filename:maybe_remove_dirsep/2                     3     0.00     0  [      0.00]
filename:major_os_type/0                           3     0.00     0  [      0.00]
filename:flatten/1                                 3     0.00     0  [      0.00]
code:ensure_loaded/1                               6     0.00     0  [      0.00]
code:call/1                                        6     0.00     0  [      0.00]
inet6_tcp:getserv/1                                1     0.00     0  [      0.00]
inet6_tcp:translate_ip/1                           1     0.00     0  [      0.00]
inet6_tcp:send/2                                 100     0.00     0  [      0.00]
inet6_tcp:listen/2                                 1     0.00     0  [      0.00]
httpd_tcp_worker_sup:start_link/0                  1     0.00     0  [      0.00]
httpd_tcp_worker_sup:init/1                        1     0.00     0  [      0.00]
error_logger_tty_h:handle_event/2                  2     0.00     0  [      0.00]
error_logger_tty_h:tag_event/1                     2     0.00     0  [      0.00]
error_logger_tty_h:do_write_event/2                2     0.00     0  [      0.00]
gen_event:notify/2                                 2     0.00     0  [      0.00]
gen_event:loop/5                                   2     0.00     0  [      0.00]
gen_event:fetch_msg/5                              2     0.00     0  [      0.00]
gen_event:handle_msg/5                             2     0.00     0  [      0.00]
prim_inet:open/4                                   1     0.00     0  [      0.00]
prim_inet:open/6                                   1     0.00     0  [      0.00]
prim_inet:enc_family/1                             1     0.00     0  [      0.00]
prim_inet:enc_type/1                               1     0.00     0  [      0.00]
prim_inet:protocol2drv/1                           1     0.00     0  [      0.00]
prim_inet:bind/3                                   2     0.00     0  [      0.00]
prim_inet:listen/2                                 1     0.00     0  [      0.00]
prim_inet:recv/3                                 102     0.00     0  [      0.00]
prim_inet:setopts/2                                2     0.00     0  [      0.00]
prim_inet:is_sockopt_val/2                         3     0.00     0  [      0.00]
prim_inet:enc_opt/1                                3     0.00     0  [      0.00]
prim_inet:type_value_default/3                     7     0.00     0  [      0.00]
prim_inet:type_value_1/3                           7     0.00     0  [      0.00]
prim_inet:enc_value/3                              4     0.00     0  [      0.00]
prim_inet:enc_value_default/3                      4     0.00     0  [      0.00]
prim_inet:enc_value_1/3                            4     0.00     0  [      0.00]
prim_inet:enc_value_2/2                            4     0.00     0  [      0.00]
prim_inet:encode_opt_val/1                         2     0.00     0  [      0.00]
prim_inet:enc_opt_val/2                            5     0.00     0  [      0.00]
prim_inet:enc_opt_val/4                            3     0.00     0  [      0.00]
prim_inet:ip6_to_bytes/1                           1     0.00     0  [      0.00]
httpd_tcp_listener:start_link/0                    1     0.00     0  [      0.00]
httpd_tcp_listener:init/0                          1     0.00     0  [      0.00]
httpd_tcp_listener:response/1                    100     0.00     0  [      0.00]
httpd_tcp_listener:'-init/0-fun-0-'/1            100     0.00     0  [      0.00]
httpd_tcp_listener:'-start_link/0-fun-0-'/0        1     0.00     0  [      0.00]
erlang:spawn_link/1                                1     0.00     0  [      0.00]
erlang:spawn_opt/4                                 2     0.00     0  [      0.00]
inet:listen_options/0                              1     0.00     0  [      0.00]
inet:listen_options/2                              1     0.00     0  [      0.00]
inet:list_add/5                                    3     0.00     0  [      0.00]
inet:tcp_module/1                                  1     0.00     0  [      0.00]
inet:tcp_module_1/2                                1     0.00     0  [      0.00]
inet:add_opt/4                                     3     0.00     0  [      0.00]
inet:translate_ip/2                                1     0.00     0  [      0.00]
inet:mod/4                                         1     0.00     0  [      0.00]
inet:mod/7                                         1     0.00     0  [      0.00]
inet:open/8                                        1     0.00     0  [      0.00]
inet:bind/3                                        1     0.00     0  [      0.00]
proc_lib:spawn_opt/4                               2     0.00     0  [      0.00]
proc_lib:check_for_monitor/1                       2     0.00     0  [      0.00]
proc_lib:ensure_link/1                             2     0.00     0  [      0.00]
proc_lib:init_p/5                                  2     0.00     0  [      0.00]
proc_lib:init_p_do_apply/3                         2     0.00     0  [      0.00]
proc_lib:start_link/5                              2     0.00     0  [      0.00]
proc_lib:sync_wait/2                               2     0.00     0  [      0.00]
proc_lib:init_ack/2                                2     0.00     0  [      0.00]
proc_lib:trans_init/3                              2     0.00     0  [      0.00]
proc_lib:get_my_name/0                             2     0.00     0  [      0.00]
proc_lib:get_ancestors/0                           2     0.00     0  [      0.00]
proc_lib:proc_info/2                               2     0.00     0  [      0.00]
gen_server:start_link/4                            2     0.00     0  [      0.00]
gen_server:call/3                                  1     0.00     0  [      0.00]
gen_server:init_it/6                               2     0.00     0  [      0.00]
gen_server:loop/6                                  2     0.00     0  [      0.00]
error_logger:info_report/2                         2     0.00     0  [      0.00]
error_logger:notify/1                              2     0.00     0  [      0.00]
application_controller:get_env/2                   1     0.00     0  [      0.00]
supervisor:start_link/3                            2     0.00     0  [      0.00]
supervisor:init/1                                  2     0.00     0  [      0.00]
supervisor:init_children/2                         1     0.00     0  [      0.00]
supervisor:init_dynamic/2                          1     0.00     0  [      0.00]
supervisor:start_children/2                        1     0.00     0  [      0.00]
supervisor:start_children/3                        3     0.00     0  [      0.00]
supervisor:init_state/4                            2     0.00     0  [      0.00]
supervisor:set_flags/2                             2     0.00     0  [      0.00]
supervisor:check_flags/1                           4     0.00     0  [      0.00]
supervisor:do_check_flags/1                        2     0.00     0  [      0.00]
supervisor:validStrategy/1                         2     0.00     0  [      0.00]
supervisor:validIntensity/1                        2     0.00     0  [      0.00]
supervisor:validPeriod/1                           2     0.00     0  [      0.00]
supervisor:supname/2                               2     0.00     0  [      0.00]
supervisor:check_startspec/1                       2     0.00     0  [      0.00]
supervisor:check_startspec/2                       5     0.00     0  [      0.00]
supervisor:do_check_childspec/1                    3     0.00     0  [      0.00]
supervisor:validChildType/1                        3     0.00     0  [      0.00]
supervisor:validName/1                             3     0.00     0  [      0.00]
supervisor:validFunc/1                             3     0.00     0  [      0.00]
supervisor:validRestartType/1                      3     0.00     0  [      0.00]
supervisor:validShutdown/1                         3     0.00     0  [      0.00]
supervisor:report_progress/2                       2     0.00     0  [      0.00]
erl_prim_loader:get_cwd/0                          3     0.00     0  [      0.00]
erl_prim_loader:handle_get_cwd/2                   3     0.00     0  [      0.00]
erl_prim_loader:efile_get_cwd/2                    3     0.00     0  [      0.00]
erl_prim_loader:'-handle_get_cwd/2-fun-0-'/2       3     0.00     0  [      0.00]
gen_tcp:listen/2                                   1     0.00     0  [      0.00]
lists:keymember/3                                  3     0.00     0  [      0.00]
lists:member/2                                     7     0.00     0  [      0.00]
erlang:put/2                                       4     0.00     0  [      0.00]
erlang:process_info/2                              2     0.00     0  [      0.00]
erlang:process_flag/2                              2     0.00     0  [      0.00]
erlang:group_leader/0                              2     0.00     0  [      0.00]
prim_file:get_cwd_int/2                            3     0.02     1  [      0.33]
lists:keydelete3/3                                 7     0.02     1  [      0.14]
error_handler:undefined_function/3                 6     0.02     1  [      0.17]
error_handler:ensure_loaded/1                      6     0.02     1  [      0.17]
code_server:is_sticky/2                            6     0.02     1  [      0.17]
code_server:try_load_module_1/5                    6     0.02     1  [      0.17]
code_server:try_load_module_3/6                    6     0.02     1  [      0.17]
code_server:post_beam_load/3                       6     0.02     1  [      0.17]
code_server:ensure_loaded/3                        6     0.02     1  [      0.17]
code_server:handle_pending_on_load/4               6     0.02     1  [      0.17]
code_server:'-try_load_module_3/6-fun-0-'/5        6     0.02     1  [      0.17]
filename:unix_pathtype/1                          12     0.02     1  [      0.08]
error_logger_tty_h:parse_event/1                   2     0.02     1  [      0.50]
gen_event:server_notify/4                          6     0.02     1  [      0.17]
gen_event:server_update/4                          4     0.02     1  [      0.25]
prim_inet:type_opt/2                               6     0.02     1  [      0.17]
prim_inet:type_opt_1/1                             6     0.02     1  [      0.17]
prim_inet:type_value/3                             7     0.02     1  [      0.14]
inet:list_opt/3                                    6     0.02     1  [      0.17]
inet:mod/6                                         6     0.02     1  [      0.17]
init:objfile_extension/0                          13     0.02     1  [      0.08]
timer:sleep/1                                      1     0.02     1  [      1.00]
error_logger:handle_event/2                        2     0.02     1  [      0.50]
supervisor:do_start_child/2                        2     0.02     1  [      0.50]
supervisor:check_childspec/1                       6     0.02     1  [      0.17]
supervisor:extract_child/1                         2     0.02     1  [      0.50]
erl_prim_loader:prim_get_cwd/2                     3     0.02     1  [      0.33]
maps:merge/2                                       5     0.02     1  [      0.20]
prim_file:internal_native2name/1                   3     0.02     1  [      0.33]
lists:keyfind/3                                   12     0.02     1  [      0.08]
ets:lookup/2                                       1     0.02     1  [      1.00]
erlang:universaltime/0                             2     0.02     1  [      0.50]
erlang:spawn_link/3                                1     0.02     1  [      1.00]
erlang:register/2                                  2     0.02     1  [      0.50]
erlang:function_exported/3                         6     0.02     1  [      0.17]
gen:timeout/1                                      2     0.04     2  [      1.00]
prim_file:drv_command/3                           16     0.04     2  [      0.13]
code_server:handle_on_load/5                       6     0.04     2  [      0.33]
gen_event:send/2                                   2     0.04     2  [      1.00]
prim_inet:recv0/3                                102     0.04     2  [      0.02]
prim_inet:enum_val/2                               9     0.04     2  [      0.22]
erlang:load_module/2                               6     0.04     2  [      0.33]
init:archive_extension/0                          13     0.04     2  [      0.15]
supervisor:validMods/1                             3     0.04     2  [      0.67]
erl_prim_loader:check_file_result/3               16     0.04     2  [      0.13]
erl_prim_loader:efile_get_file_from_port2/2       13     0.04     2  [      0.15]
erl_prim_loader:'-handle_get_file/3-fun-0-'/3     13     0.04     2  [      0.15]
prim_file:close/1                                 13     0.06     3  [      0.23]
prim_file:drv_close/1                             16     0.06     3  [      0.19]
prim_file:drv_command/2                           13     0.06     3  [      0.23]
prim_file:drv_get_response/1                      16     0.06     3  [      0.19]
prim_file:'-drv_command/4-after$^0/0-0-'/1        16     0.06     3  [      0.19]
prim_file:'-drv_close/1-after$^0/0-0-'/1          16     0.06     3  [      0.19]
filename:append/2                                 13     0.06     3  [      0.23]
erl_prim_loader:handle_get_file/3                 13     0.06     3  [      0.23]
erl_prim_loader:efile_get_file_from_port/3        13     0.06     3  [      0.23]
erl_prim_loader:is_basename/1                     13     0.06     3  [      0.23]
erlang:iolist_size/1                              13     0.06     3  [      0.23]
lists:reverse/2                                   18     0.06     3  [      0.17]
erlang:port_get_data/1                           202     0.06     3  [      0.01]
erlang:list_to_atom/1                              7     0.06     3  [      0.43]
prim_file:read_file/2                             13     0.08     4  [      0.31]
prim_file:drv_command/4                           19     0.08     4  [      0.21]
prim_file:drv_get_response/2                      16     0.08     4  [      0.25]
prim_file:translate_response/2                    16     0.08     4  [      0.25]
prim_file:pathname/1                              13     0.08     4  [      0.31]
code_server:handle_call/3                          6     0.08     4  [      0.67]
code_server:mod_to_bin/2                          13     0.08     4  [      0.31]
code_server:objfile_extension/0                   13     0.08     4  [      0.31]
filename:pathtype/1                               12     0.08     4  [      0.33]
os:type/0                                         15     0.08     4  [      0.27]
prim_inet:type_value_2/2                           9     0.08     4  [      0.44]
erlang:open_port/2                                17     0.08     4  [      0.24]
erlang:port_command/2                             16     0.08     4  [      0.25]
erl_prim_loader:get_file/1                        13     0.08     4  [      0.31]
erl_prim_loader:request/1                         16     0.08     4  [      0.25]
erl_prim_loader:prim_get_file/2                   13     0.08     4  [      0.31]
erl_prim_loader:reverse/1                         13     0.08     4  [      0.31]
erl_prim_loader:name_split/2                      13     0.08     4  [      0.31]
prim_file:internal_name2native/1                  13     0.08     4  [      0.31]
erlang:spawn_opt/1                                 2     0.08     4  [      2.00]
erlang:setelement/3                               34     0.08     4  [      0.12]
erlang:atom_to_list/1                             13     0.08     4  [      0.31]
prim_file:drv_open/2                              16     0.10     5  [      0.31]
lists:seq_loop/3                                  26     0.10     5  [      0.19]
erlang:port_command/3                            100     0.10     5  [      0.05]
erlang:system_info/1                              21     0.10     5  [      0.24]
erlang:'++'/2                                     26     0.12     6  [      0.23]
erlang:port_set_data/2                           103     0.12     6  [      0.06]
erlang:module_loaded/1                            18     0.12     6  [      0.33]
httpd_tcp_listener:process/3                     102     0.14     7  [      0.07]
erlang:port_close/1                              118     0.14     7  [      0.06]
erl_prim_loader:deep_member/2                     52     0.14     7  [      0.13]
erlang:demonitor/2                                 7     0.14     7  [      1.00]
prim_inet:send/3                                 100     0.17     9  [      0.09]
gen_tcp:send/2                                   100     0.17     9  [      0.09]
inet6_tcp:recv/3                                 102     0.19    10  [      0.10]
erlang:spawn/1                                   100     0.19    10  [      0.10]
prim_file:read_file/1                             13     0.21    11  [      0.85]
gen_tcp:recv/3                                   102     0.21    11  [      0.11]
ets:insert/2                                       6     0.21    11  [      1.83]
code_server:reply/2                                6     0.25    13  [      2.17]
inet_db:register_socket/2                        103     0.27    14  [      0.14]
erl_prim_loader:handle_request/3                  16     0.27    14  [      0.88]
erlang:monitor/2                                   7     0.27    14  [      2.00]
erlang:send/3                                      1     0.29    15  [     15.00]
erlang:whereis/1                                  25     0.35    18  [      0.72]
erl_prim_loader:debug/2                           32     0.37    19  [      0.59]
erlang:bump_reductions/1                          16     0.37    19  [      1.19]
lists:foreach/2                                  104     0.39    20  [      0.19]
httpd_tcp_listener:'-init/0-fun-1-'/2            100     0.41    21  [      0.21]
prim_inet:async_recv/3                           102     0.52    27  [      0.26]
inet_db:lookup_socket/1                          202     0.56    29  [      0.14]
erl_prim_loader:loop/3                            16     0.58    30  [      1.88]
filename:join1/4                                 302     0.60    31  [      0.10]
erl_prim_loader:is_prefix/2                       78     0.64    33  [      0.42]
erlang:apply/2                                   101     0.66    34  [      0.34]
erts_internal:open_port/2                         17     0.70    36  [      2.12]
filename:do_flatten/2                            155     1.05    54  [      0.35]
erl_prim_loader:archive_split/3                  567     1.28    66  [      0.12]
prim_inet:ctl_cmd/3                              310     1.44    74  [      0.24]
erlang:spawn/3                                   100     1.55    80  [      0.80]
prim_inet:enc_time/1                             305     1.63    84  [      0.28]
erlang:port_control/3                            310     2.39   123  [      0.40]
httpd_tcp_listener:accept/1                      203     2.42   125  [      0.62]
code_server:loop/1                                 6     3.43   177  [     29.50]
prim_inet:async_accept/2                         203     3.59   185  [      0.91]
erts_internal:port_close/1                       118    10.88   561  [      4.75]
erlang:prepare_loading/2                           6    12.30   634  [    105.67]
erts_internal:port_control/3                     310    12.70   655  [      2.11]
erlang:finish_loading/1                            6    13.91   717  [    119.50]
erts_internal:port_command/3                     116    17.61   908  [      7.83]
---------------------------------------------  -----  -------  ----  [----------]
Total:                                          6854  100.00%  5156  [      0.75]
```

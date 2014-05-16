

-module(ack).


-export([r/0]).
-export([r/1]).

-export([start_ets/1]).
-export([start_ets/2]).
-export([stop_ets/0]).

-export([start_now/1]).
-export([stop_now/0]).

-export([start_periodically_check_multi_stuff/0]).
-export([stop_periodically_check_multi_stuff/0]).

-export([start_cpu_utilisation/0]).
-export([start_cpu_utilisation/1]).
-export([stop_cpu_utilisation/0]).

-export([print_system_info/0]).




r() -> r(2).
r(Sz) when is_integer(Sz) -> r([{m,3}, {n,Sz}]);
r(Opts) when is_list(Opts) ->
    M = gv(m, Opts, 3),
    N = gv(n, Opts, 2),
    P = gv(p, Opts, 1),
    T1 = now(),
    _L=[start_ack(X,M,N) || X <- lists:seq(1, P)],
    _L2=[get_ack_result(X) || X <- lists:seq(1, P)],
    Time = timer:now_diff(now(), T1),
    io:format("ack(~p, ~p) in ~p procs took: ~pms~n", 
	      [M, N, P, Time div 1000]).

    

start_ack(Id,M,N) ->
    Self = self(),
    spawn(fun() -> Self ! {Id, ack(M,N)} end).

get_ack_result(Id) -> 
    receive Res={Id, _} -> Res end.


ack(0,N) -> N+1;
ack(M, 0) when M>0 -> ack(M-1, 1);
ack(M, N) when M>0, N>0 -> ack(M-1, ack(M,N-1)).


start_periodically_check_multi_stuff() ->
    spawn(fun() -> init_periodically_check_multi_stuff() end).

stop_periodically_check_multi_stuff() ->
    periodically_check_multi_stuff ! stop. 

init_periodically_check_multi_stuff() ->
    register(periodically_check_multi_stuff, self()),
    periodically_check_multi_stuff().

periodically_check_multi_stuff() ->
    check_multi_stuff(),
    receive stop -> ok 
    after 100 -> periodically_check_multi_stuff()
    end.

check_multi_stuff() ->
    case erlang:system_info(multi_scheduling_blockers) of
	[] -> ok;
	L ->
	    Fmt = "~p multi_scheduling_blockers (~p) ~p~n",
	    case length(L) of
		N when N < 5 -> io:format(Fmt, [now(), N, L]);
		N -> io:format(Fmt, [now(), N, "..."])
	    end
    end.


%% erlang:system_info(scheduler_bindings).
%% erlang:system_info(port_parallelism).
%% erlang:system_info(multi_scheduling_blockers).
%% erlang:system_info(multi_scheduling).

print_system_info() ->
    Ts = [scheduler_bindings,
	  port_parallelism,
	  multi_scheduling_blockers,
	  multi_scheduling],
    Fmt = lists:flatten(["   ~p: ~p~n" || _ <- Ts]),
    Res = lists:foldl(fun(T, A) ->
			      A++([T]++[get_si(T)])
		      end,
		      [], Ts),
    
    io:format("system info~n"++Fmt, Res).

    

get_si(Tag) ->
    case catch erlang:system_info(Tag) of
	Err when element(1, Err) == 'EXIT' -> error;
	Res -> Res
    end.


%% +sbt db +sub true
%% +sbwt very_short 
%% +sbwt short 
%% +sbwt long 


%%---------------
%% Start checking cpu utilization
start_cpu_utilisation() ->
    start_cpu_utilisation(2000).
start_cpu_utilisation(T) ->
    erlang:system_flag(scheduler_wall_time, true),
    InitTs = get_scheduler_wall_time(),
    Fun = fun(Ts0) ->
		  Ts1 = get_scheduler_wall_time(),
		  Util = calc_scheduler_utilization(Ts0, Ts1),
		  print_utilization(Util),
		  Ts1
	  end,
    start_periodical(cpu_util, T, Fun, InitTs).

stop_cpu_utilisation() ->
    stop_periodical(cpu_util).


print_utilization(L) ->
    maybe_print_cpu_util_header(L),
    Fmt = "  ~6.2f"++lists:flatten(["  ~4.2f" || _ <- tl(L)]),
    Args = lists:flatten([element(2,T) || T<- L]),
    io:format(Fmt++"~n", Args).

maybe_print_cpu_util_header(L) ->
    maybe_print_cpu_util_header(should_we_print_header(), L).
maybe_print_cpu_util_header(true, L) ->
    Fmt = " ~6w "++lists:flatten(["  ~3w " || _ <- tl(L)]),
    Args = lists:flatten([element(1,T) || T<- L]),
    io:format(Fmt++"~n", Args);
maybe_print_cpu_util_header(_, _) ->
    ok.


should_we_print_header() ->
    OldC = case get(cpu_util_count) of
	       N when is_integer(N) -> N;
	       _ -> 0
	   end,
    put(cpu_util_count, OldC+1),
    (OldC rem 10) == 0.


calc_scheduler_utilization(Ts0, Ts1) ->
    lists:map(fun({{I, A0, T0}, {I, A1, T1}}) ->
		      {I, safe_div(A1 - A0, T1 - T0)} end, 
	      lists:zip(Ts0,Ts1)).

safe_div(_, 0) -> 0.0;
safe_div(A,B) -> A/B.

get_scheduler_wall_time() -> 
    [get_wall_utilization() | 
     lists:sort(erlang:statistics(scheduler_wall_time))].

get_wall_utilization() ->
    {total, 
     element(1, statistics(runtime)), 
     element(1,statistics(wall_clock))}.



%%---------------
%% Start a peridical job
start_periodical(Name, T, Fun, InitVal) ->
    spawn(fun() -> init_periodical(Name, T, Fun, InitVal) end).

stop_periodical(Name) ->
    Name ! stop. 

init_periodical(Name, T, Fun, InitVal) ->
    register(Name, self()),
    periodical_loop(Name, T, Fun, InitVal).


periodical_loop(Name, T, Fun, State) ->
    NewState = Fun(State),
    receive stop -> ok 
    after T -> periodical_loop(Name, T, Fun, NewState)
    end.



%%---------------
%% ETS locking stuff

start_ets(N) ->
    start_ets(N, []).
start_ets(N, Opts) ->
    start_procs(N, Opts).

stop_ets() ->
    stop_procs().


init_ets(N) ->
    io:format("ets ~p started~n", [N]),
    loop_ets(N).

loop_ets(N) ->
    ets_inc(counter),
    _V = ets_lookup(n),
%%    erlang:yield(),
    receive stop -> io:format("ets ~p stopped~n", [N]), ets_del({proc, N}), ok
    after 0 -> loop_ets(N)
    end.


%% start_ets(N) ->
%%     start_ets(N, []).
%% start_ets(N, Opts) ->
%%     catch ets:delete(tab1),
%%     ets:new(tab1, [set, public, named_table|gv(ets_opts, Opts, [])]),
%%     ets_ins(n, N),
%%     ets_ins(start_time, now()),
%%     ets_ins(counter, 0),
%%     start_ets2(Opts).

%% start_ets2(Opts) -> start_ets2(ets_lookup(n), Opts).
%% start_ets2(N, Opts) when is_integer(N), N > 0 -> 
%%     Fun = case gv(use_ack, Opts, false) of
%% 	      true -> fun() -> init_ets_ack(N) end;
%% 	      _ ->    fun() -> init_ets(N) end
%% 	  end,
%%     ets_ins({proc, N}, spawn(Fun)),
%%     start_ets2(N-1, Opts);
%% start_ets2(_, _) ->
%%     ok.

%% stop_ets() -> 
%%     StopTime = now(),
%%     stop_ets(ets_lookup(n)),
%%     ets_lookup(counter) / 
%% 	(timer:now_diff(StopTime, ets_lookup(start_time)) / 1000000).
%% stop_ets(N) when is_integer(N), N > 0 ->
%%     stop_ets(ets_lookup({proc, N})),
%%     stop_ets(N-1);
%% stop_ets(Pid) when is_pid(Pid) -> 
%%     Pid ! stop;
%% stop_ets(_) ->
%%     ok.


%% init_ets(N) ->
%%     io:format("ets ~p started~n", [N]),
%%     loop_ets(N).

%% loop_ets(N) ->
%%     ets_inc(counter),
%%     _V = ets_lookup(n),
%% %%    erlang:yield(),
%%     receive stop -> io:format("ets ~p stopped~n", [N]), ets_del({proc, N}), ok
%%     after 0 -> loop_ets(N)
%%     end.


init_ets_ack(N) ->
    io:format("ets ~p started~n", [N]),
    loop_ets_ack(N).

loop_ets_ack(N) ->
    ets_inc(counter),
    ack(2,2),
    _V = ets_lookup(n),
    receive stop -> io:format("ets ~p stopped~n", [N]), ets_del({proc, N}), ok
    after 0 -> loop_ets_ack(N)
    end.


ets_ins(K,V) -> ets:insert(tab1, {K, V}).
ets_inc(K) -> ets:update_counter(tab1, K, 1).
ets_del(K) -> ets:delete(tab1, K).
ets_lookup(K) -> 
    case ets:lookup(tab1, K) of
	[{_, V}] -> V;
	_ -> undefined
    end.


%%---------------
%% now/0 locking stuff
start_now(N) ->
    start_now(N, []).
start_now(N, Opts) ->
    start_procs(N, [{method, now}|Opts]).

stop_now() ->
    stop_procs().


init_now(N) ->
    io:format("now ~p started~n", [N]),
    loop_now(N).

loop_now(N) ->
    now(),
    receive stop -> io:format("now ~p stopped~n", [N]), ets_del({proc, N}), ok
    after 0 -> loop_now(N)
    end.



%%---------------
%% procs framework

start_procs(N, Opts) ->
    catch ets:delete(tab1),
    ets:new(tab1, [set, public, named_table|gv(ets_opts, Opts, [])]),
    ets_ins(n, N),
    ets_ins(start_time, now()),
    ets_ins(counter, 0),
    start_procs2(Opts).

start_procs2(Opts) -> start_procs2(ets_lookup(n), Opts).
start_procs2(N, Opts) when is_integer(N), N > 0 -> 
    SpawnFun = get_spawn_function(N, Opts),
    ets_ins({proc, N}, spawn(SpawnFun)),
    start_procs2(N-1, Opts);
start_procs2(_, _) ->
    ok.

get_spawn_function(N, Opts) ->
    get_spawn_function2(N, gv(method, Opts)).
get_spawn_function2(N, now) -> fun() -> init_now(N) end;
get_spawn_function2(N, ets_ack) -> fun() -> init_ets_ack(N) end;
get_spawn_function2(N, _) -> fun() -> init_ets(N) end.


stop_procs() -> 
    StopTime = now(),
    stop_procs(ets_lookup(n)),
    ets_lookup(counter) / 
	(timer:now_diff(StopTime, ets_lookup(start_time)) / 1000000).
stop_procs(N) when is_integer(N), N > 0 ->
    stop_procs(ets_lookup({proc, N})),
    stop_procs(N-1);
stop_procs(Pid) when is_pid(Pid) -> 
    Pid ! stop;
stop_procs(_) ->
    ok.


%%---------------
%% UDP socket stuff
%% start_udp(Opts) ->
%%     [Mate|_] = nodes(),
%%     MyIp = gv(my_ip, Opts, 
    



%%---------------
%% Got tired of the long proplists:get_value
gv(T, L) -> proplists:get_value(T,L).
gv(T, L, D) -> proplists:get_value(T,L, D).
%% sv(K, V, L) -> lists:keystore(K, 1, L, {K, V}).
%% dv(K, L) -> lists:keydelete(K, 1, L).

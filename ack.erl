

-module(ack).


-export([r/0]).
-export([r/1]).
-export([run/1]).
-export([run/2]).
-export([run/3]).
-export([stop_run/0]).

-export([measure/1]).
-export([measure/3]).

%% To see available methods/tests
-export([which_methods/0]).


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





%%---------------
%% Ackerman 
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

    

%%---------------
%% Ackerman stuff
start_ack(Id,M,N) ->
    Self = self(),
    spawn(fun() -> Self ! {Id, ack(M,N)} end).

get_ack_result(Id) -> 
    receive Res={Id, _} -> Res end.


ack(0,N) -> N+1;
ack(M, 0) when M>0 -> ack(M-1, 1);
ack(M, N) when M>0, N>0 -> ack(M-1, ack(M,N-1)).


%%---------------
%% Load test stuff, deprecated use run/2/3 instead
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
    %% L contains one entry for each core, plus one for the total
    Ncores = length(L) -1, 
    Fmt = "  ~6.2f (~2w%)"++lists:flatten(["  ~4.2f" || _ <- tl(L)]),
    [Tot|Args] = lists:flatten([element(2,T) || T<- L]),
    io:format(Fmt++"~n", [Tot, round((100*Tot) / Ncores) | Args]).

maybe_print_cpu_util_header(L) ->
    maybe_print_cpu_util_header(should_we_print_header(), L).
maybe_print_cpu_util_header(true, L) ->
    Fmt = " ~12w "++lists:flatten(["  ~3w " || _ <- tl(L)]),
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
%% Load test stuff, deprecated use run/2/3 instead

start_ets(N) -> run(N, ets).
start_ets(N, Opts) -> run(N, ets, Opts).
stop_ets() -> stop_procs().

start_now(N) -> run(N, now).
stop_now() -> stop_procs().


%% no_lock, old framework
%% 4 cores  116.293.893
%% 8 cores  143.724.169
%% -- new framework
%%  4 cores  49.871.501
%%  8 cores  94.768.343
%% 16 cores 105.608.994
%%
%% -- os:timestamp
%%  4 cores  39.672.391
%%  8 cores  49.608.262
%% 16 cores  48.353.703
%%
%% -- ets
%%  1 cores  5.442.277
%%  4 cores  2.340.654
%%  8 cores  1.110.055
%% 16 cores  1.130.878
%%
%% -- proc_ets
%%  1 cores  721.987
%%  4 cores  366.991
%%  8 cores  369.710
%% 16 cores  328.275
%%
%% -- proc_ets
%%  1 cores  721.987
%%  4 cores  366.991
%%  8 cores  369.710
%% 16 cores  328.275
%%
%% -- timer
%%  1 cores   1.987.127
%%  4 cores     881.387
%%  8 cores     459.290
%% 16 cores     451.397
%%
%% -- sysTimer2 (one server)
%%  1 cores     184.184
%%  4 cores     210.138
%%  8 cores     203.343
%% 16 cores     163.310
[
measure(Method) ->
    print_measure(Method, measure(Method, [1,4,8,16], 5000)).

measure(Method, ProcSpecList, Timeout) ->
    [do_measure(Method, ProcSpec, Timeout) || ProcSpec <- ProcSpecList].

do_measure(Method, ProcSpec, Timeout) ->
    run(ProcSpec, Method),
    timer:sleep(Timeout),
    {ProcSpec, stop_run()}.


print_measure(Method, L) ->
    io:format("%%~n%% -- ~p~n", [Method]),
    [io:format("%% ~2w cores ~s~n", [C, nice_i2l(V)]) || {C,V} <- L].

nice_i2l(Int) ->
    Str = rev(put_dots_where_they_should_be(rev(integer_to_list(Int)))),
    string:right(Str, 11).

put_dots_where_they_should_be([A,B,C|R]) when R/= [] ->
    [A,B,C,$.|put_dots_where_they_should_be(R)];
put_dots_where_they_should_be(R) ->
    R.

rev(L) -> lists:reverse(L).

%%---------------
%% procs framework

run(N) when is_integer(N) ->
    run(N, ets).
run(N, Method) ->
    run(N, Method, []).
run(N, Method, Opts) when is_integer(N), is_atom(Method), is_list(Opts) ->
    start_procs(N, Method, Opts).

which_methods() -> [element(1, M) || M <- method_definition_list()].

start_procs(N, Method, Opts) ->
    new_ets_tab(tab1),
    ets_ins(n, N),
    ets_ins(start_time, now()),
    ets_ins(counter, 0),
    {InitFun, Fun} = get_method_definition(Method),
    InitFun(Opts),
    start_procs2(N, Fun, Opts).

start_procs2(Id, Fun, Opts) when is_integer(Id), Id > 0 -> 
    ets_ins({proc, Id}, spawn(fun() -> init_procs(Id, Fun) end)), 
    start_procs2(Id-1, Fun, Opts);
start_procs2(_, _, _) ->
    ok.

stop_run() ->
    stop_procs().

get_method_definition(Method) ->
    case gv(Method, method_definition_list()) of
	Res = {_InitFun, _Fun} -> Res;
	Fun -> {fun(_) -> ok end, Fun}
    end.


method_definition_list() ->
    [{os_now, fun() -> os:timestamp() end},
     {now, fun() -> now() end},
     {no_lock, fun() -> ok end},
     {ets_one_proc, {fun(_) -> proc_ets_new(tab2) end,
      fun() -> proc_ets_inc(tab2, counter) end}},
     {timer, fun() -> T = erlang:send_after(1000, self(), hej), 
		      erlang:cancel_timer(T) end},
     {sysTimer2, {fun(_) -> catch sysTimer2:stop(),
			    sysTimer2:start() end,
		  fun() -> T = sysTimer2:send_after(1000, self(), hej), 
			   sysTimer2:cancel_timer(T) end}},
     {ets, fun() -> ets_inc(counter) end}].


init_procs(Id, Fun) ->
%%    io:format("proc~p started~n", [Id]),
    %% start at 1 because we'll do at least 1 before being stopped
    loop_procs(Id, 1, Fun).

loop_procs(Id, Count, Fun)->
    Fun(),
    receive {stop, From} ->
	    acknowledge_stop(From, Id, Count)
    after 0 -> loop_procs(Id, Count+1, Fun)
    end.


acknowledge_stop(From, Id, Count) ->
%%    io:format("proc ~p stopped~n", [Id]), 
    From ! {ack, {Id, Count}}.
    
stop_procs() -> 
    StopTime = now(),
    send_stop_procs(ets_lookup(n)),
    Res1 = gather_procs_return(ets_lookup(n)),
    round(Res1 / (timer:now_diff(StopTime, ets_lookup(start_time)) / 1000000)).

send_stop_procs(N) when is_integer(N), N > 0 ->
    case ets_lookup({proc, N}) of
	Pid when is_pid(Pid) -> Pid ! {stop, self()};
	_ -> ok
    end,
    send_stop_procs(N-1);
send_stop_procs(_) ->
    ok.

gather_procs_return(N) when is_integer(N), N > 0 ->
    case ets_lookup({proc, N}) of
	Pid when is_pid(Pid) -> 
	    ets_del({proc, N}),
	    receive {ack, {N, Res}} -> Res+gather_procs_return(N-1)
	    after 20000 -> gather_procs_return(N-1)
	    end;
	_ -> gather_procs_return(N-1)
    end;
gather_procs_return(_) ->
    0.


%%---------------
%% ETS table primitives
new_ets_tab(T) ->
    new_ets_tab(T, [set, named_table, public]).
new_ets_tab(T, Opts) ->
    catch ets:delete(T),
    ets:new(T, Opts).

ets_ins(K,V) -> ets_ins(tab1,K,V).
ets_ins(T,K,V) -> ets:insert(T, {K, V}).
ets_inc(K) -> ets_inc(tab1, K).
ets_inc(T,K) -> ets:update_counter(T, K, 1).
ets_del(K) -> ets_del(tab1, K).
ets_del(T,K) -> ets:delete(T, K).
ets_lookup(K) -> ets_lookup(tab1, K).
ets_lookup(T,K) -> 
    case ets:lookup(T, K) of
	[{_, V}] -> V;
	_ -> undefined
    end.



%%---------------
%% ETS table as a process primitives
proc_ets_new(T) ->
    stop_proc_ets(T),
    spawn(fun() -> init_proc_ets(T) end), timer:sleep(100).

stop_proc_ets(T) ->
    case whereis(T) of
	Pid when is_pid(Pid) -> do_call(Pid, stop);
	_ -> ok
    end.
	
init_proc_ets(T) ->
    register(T,self()),
    new_ets_tab(T),
    ets_ins(T, counter, 0),
    proc_ets_loop(T).

proc_ets_loop(stop) ->
    ok;
proc_ets_loop(T) ->
    receive
	{call, From, Req} ->
	    proc_ets_loop(send_reply(From, handle_call(Req, T)))
    end.

send_reply(From, {Res, State}) ->
    From ! {reply, Res},
    State.

handle_call({inc, K}, T) ->
    ets_inc(T, K),
    {ok, T};
handle_call(stop, T) ->
    unregister(T),
    {ok, stop}.

			

proc_ets_inc(T,K) -> do_call(T, {inc, K}).

do_call(Pid, Req) ->
    Pid ! {call, self(), Req},
    receive
	{reply, Res} -> Res
    end.



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

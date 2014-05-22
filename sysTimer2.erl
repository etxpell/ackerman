

-module(sysTimer2).



-export([start/0]).
-export([start/1]).
-export([stop/0]).


-export([send_after/2]).
-export([send_after/3]).
-export([cancel/1]).
-export([cancel_timer/1]).
-export([get_state/0]).




-record(s, {name, time_tab, ref_tab, cancel_count=0, send_after_count=0,
	    collision_count=0, spare1, spare2, spare3}).

-record(timer, {ref, pid, msg, time, spare1, spare2, spare3}).


start() -> start([]).
start(Opts) ->
    multi_start(Opts).

multi_start(Opts) ->
    multi_start(all_servers(), Opts).
multi_start(Servers, Opts) ->
    [do_start([{name, X}|Opts]) || X <- Servers].

do_start(Opts) -> spawn(fun() -> init(Opts) end).

stop() ->
    multi_call(stop).

multi_call(Req) ->
    multi_call(all_servers(), Req).
multi_call(Servers, Req) ->
    [call(X, Req) || X <- Servers].

send_after(Time, Msg) ->
    send_after(Time, self(), Msg).
send_after(Time, Pid, Msg) ->
    send_after(Time, Pid, Msg, erlang:make_ref()).

send_after(Time, Pid, Msg, Ref) ->
    call(server_select(Ref), {send_at, mk_timer(Time, Pid, Msg, Ref)}),
    Ref.

cancel_timer(Ref) ->
    cancel(Ref).
cancel(Ref) ->
    call(server_select(Ref), {cancel, Ref}).

get_state() ->
    multi_call(get_state).



init(Opts) ->
    Name = gv(name, Opts),
    register(Name, self()),
    S = #s{name=Name, 
	   time_tab= create_time_ets(),
	   ref_tab = create_ref_ets()},
    start_tick(S),
    loop(S).

terminate(#s{name=Name}) ->    
    %% cleanup S here...
    unregister(Name),
    ok.


%%---------------
%% 

loop({stop, _S}) ->
    ok;
loop(S) ->
    receive
	{call, From, Req} ->
	    loop(send_reply(From, handle_call(Req, S)));
	tick ->
	    start_tick(S),
	    check_timeouts(S),
	    loop(S)
    end. 


handle_call(stop, S) ->
    {ok, {stop, terminate(S)}};
handle_call(get_state, S) ->
    {get_state_info(S), S};
handle_call({send_at, Timer}, S) ->
    NewS = adjust_and_set_timer_tables(Timer, S),
    {timer_ref(Timer), inc_send_after(NewS)};
handle_call({cancel, Ref}, S) ->
    get_and_del_everything_for_a_ref(Ref, S),
    {ok, inc_cancels(S)}.


call(Name, Req) when is_atom(Name) ->
    case whereis(Name) of
	Pid when is_pid(Pid) -> call(Pid, Req);
	_ -> no_proc
    end;
call(Pid, Req) ->
    Pid ! {call, self(), Req},
    receive
	{reply, Res} -> Res
    end.

send_reply(Pid, {Res, State}) ->
    Pid ! {reply, Res},
    State.


inc_collisions(S=#s{collision_count=N}) ->
    S#s{collision_count=N+1}.
inc_cancels(S=#s{cancel_count=N}) ->
    S#s{cancel_count=N+1}.
inc_send_after(S=#s{send_after_count=N}) ->
    S#s{send_after_count=N+1}.

check_timeouts(S) ->
    check_all_timeouts(timestamp(), first_time(S), S).

check_all_timeouts(Now, Timeout, S) 
  when is_tuple(Timeout), Now > Timeout ->
    send_timeout(Timeout, S),
    check_all_timeouts(Now, first_time(S), S);
check_all_timeouts(_, _,_) ->
    ok.

send_timeout(Time, S) ->
    Timer = get_and_delete_timer(Time, S),
    timer_pid(Timer) ! timer_msg(Timer).
    

get_and_delete_timer(Time, S) ->
    get_and_del_everything_for_a_ref(lookup_time(Time, S), S).

get_and_del_everything_for_a_ref(Ref, S) ->
    get_and_del_everything_for_a_timer(lookup_ref(Ref, S), S).

get_and_del_everything_for_a_timer(Timer=#timer{}, S) ->
    del_time(Timer, S),
    del_ref(Timer, S),
    Timer.


adjust_and_set_timer_tables(Timer, S) ->
    store_timer(adjust_time_to_be_unique(Timer, S)).

store_timer({Timer, S}) ->
    store_time(Timer, S),
    store_ref(Timer, S),
    S.


adjust_time_to_be_unique(Timer, S) ->
    case lookup_time(Timer, S) of
	undefined -> {Timer, S};
	_ -> adjust_time_to_be_unique(add_one_to_time(Timer), 
				      inc_collisions(S))
    end.

add_one_to_time(Timer=#timer{time=Time}) ->
    io:format("ADDING ONE!!!~n", []),
    Timer#timer{time=now_add(Time, 1)}.



mk_timer(Time, Pid, Msg, Ref) ->
    #timer{ref=Ref, pid=Pid, msg=Msg, time=calc_timeout(Time)}.



%%---------------
%% Get a nice table content
get_state_info(#s{name=Name, time_tab=Times, ref_tab=Refs, 
		  cancel_count=NC, send_after_count=NSA,
		  collision_count=NCol}) ->
    {Name, {'#cancels', NC}, {'#send_after', NSA}, {'#collisions', NCol}, 
     {time, table_info(Times)}, {ref, table_info(Refs)}}.

table_info(Tab) ->
    [{size, ets:info(Tab, size)},
     {contents, table_contents(5, Tab)}].

table_contents(N, Tab) -> table_contents(N, Tab, ets:first(Tab)).

table_contents(N, _, Key) 
  when N < 1 ; (Key=='$end_of_table') ->
    [];
table_contents(N, Tab, Key) ->
    ets:lookup(Tab, Key) ++ table_contents(N+1, Tab, ets:next(Tab, Key)).



%%---------------
%% ETS table primitives

first_time(#s{time_tab=T}) ->
    ets:first(T).
lookup_time(#timer{time=Time}, #s{time_tab=T}) ->
    lookup_time(Time, #s{time_tab=T});
lookup_time(Time, #s{time_tab=T}) ->
    case ets:lookup(T, Time) of
	[{_, Ref}] -> Ref;
	_ -> undefined
    end.
store_time(#timer{time=Time, ref=Ref}, #s{time_tab=T}) ->
    ets:insert(T, {Time, Ref}).
del_time(#timer{time=Time}, #s{time_tab=T}) ->
    ets:delete(T, Time).

lookup_ref(#timer{ref=Ref}, #s{ref_tab=T}) ->
    lookup_ref(Ref, #s{ref_tab=T});
lookup_ref(Ref, #s{ref_tab=T}) ->
    case ets:lookup(T, Ref) of
	[Timer] -> Timer;
	_ -> undefined
    end.
store_ref(Timer, #s{ref_tab=T}) ->
    ets:insert(T, Timer).
del_ref(#timer{ref=Ref}, #s{ref_tab=T}) ->
    ets:delete(T, Ref).


create_time_ets() ->
    create_ets([ordered_set, private]).
create_ref_ets() ->
    create_ets([set, private, {keypos, #timer.ref}]).

create_ets(Opts) ->
    ets:new(undefined, Opts).

timer_pid(#timer{pid=Pid}) -> Pid.
timer_msg(#timer{msg=Msg}) -> Msg.
timer_ref(#timer{ref=Ref}) -> Ref.


%%---------------
%% 
start_tick(_S) ->
    erlang:send_after(100, self(), tick).
    
%%---------------
%% 
calc_timeout(Time) ->
    now_add(timestamp(), Time*1000).

now_add({A,B,C}, Int) ->
    now_carry_c(A, B, C+Int).

now_carry_c(A,B,C) when C < 1000000 -> {A,B,C};
now_carry_c(A,B,C) ->
    now_carry_b(A, B+(C div 1000000), C rem 1000000).

now_carry_b(A,B,C) when B < 1000000 -> {A,B,C};
now_carry_b(A,B,C) ->
    {A+(B div 1000000), B rem 1000000, C}.


%%---------------
%% 
timestamp() -> os:timestamp().

%%---------------
%% Use more than one server in the future
-define(MAX_PROCS, 8).

all_servers() -> [server_select(N) || N <- lists:seq(1,?MAX_PROCS)].

server_select(Ref) when is_reference(Ref) ->  
    server_select(hash_ref(Ref));
%%server_select(X) when X =< 25 ->list_to_atom("sysTimer2_"++integer_to_list(X));
server_select(1) -> sysTimer2_1;
server_select(2) -> sysTimer2_2;
server_select(3) -> sysTimer2_3;
server_select(4) -> sysTimer2_4;
server_select(5) -> sysTimer2_5;
server_select(6) -> sysTimer2_6;
server_select(7) -> sysTimer2_7;
server_select(8) -> sysTimer2_8;
server_select(_) -> undefined.

hash_ref(Ref) ->
    erlang:phash(Ref, ?MAX_PROCS).


%%---------------
%% Got tired of the long proplists:get_value
gv(T, L) -> proplists:get_value(T,L).
%%gv(T, L, D) -> proplists:get_value(T,L, D).
%% sv(K, V, L) -> lists:keystore(K, 1, L, {K, V}).
%% dv(K, L) -> lists:keydelete(K, 1, L).

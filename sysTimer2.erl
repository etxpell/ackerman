

-module(sysTimer2).



-export([start/0]).
-export([start/1]).
-export([stop/0]).


-export([send_after/2]).
-export([send_after/3]).
-export([cancel/1]).
-export([cancel_timer/1]).
-export([get_state/0]).




-record(s, {name, time_tab, ref_tab}).

-record(timer, {ref, pid, msg, time}).


start() -> start([]).
start(Opts) -> spawn(fun() -> init(Opts) end).

stop() ->
    call(main_server(), stop).

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
    call(server_select(undefined), get_state).

init(Opts) ->
    Name = gv(name, Opts, main_server()),
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
    {get_table_info(S), S};
handle_call({send_at, Timer}, S) ->
    store_timer(Timer, S),
    {timer_ref(Timer), S};
handle_call({cancel, Ref}, S) ->
    get_and_del_everything_for_a_ref(Ref, S),
    {ok, S}.


call(Pid, Req) ->
    Pid ! {call, self(), Req},
    receive
	{reply, Res} -> Res
    end.

send_reply(Pid, {Res, State}) ->
    Pid ! {reply, Res},
    State.

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


store_timer(Timer, S) ->
    store_timer2(adjust_time_to_be_unique(Timer, S), S).

store_timer2(Timer, S) ->
    store_time(Timer, S),
    store_ref(Timer, S).


adjust_time_to_be_unique(Timer, S) ->
    case lookup_time(Timer, S) of
	undefined -> Timer;
	_ -> adjust_time_to_be_unique(add_one_to_time(Timer), S)
    end.

add_one_to_time(Timer=#timer{time=Time}) ->
    Timer#timer{time=now_add(Time, 1)}.



mk_timer(Time, Pid, Msg, Ref) ->
    #timer{ref=Ref, pid=Pid, msg=Msg, time=calc_timeout(Time)}.



%%---------------
%% Get a nice table content
get_table_info(#s{time_tab=Times, ref_tab=Refs}) ->
    {table_info(time, Times), table_info(ref, Refs)}.

table_info(Name, Tab) ->
    [{name, Name}, {size, ets:info(Tab, size)} | 
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
main_server() -> sysTimer2.
server_select(_) ->  main_server().

%%---------------
%% Got tired of the long proplists:get_value
%%gv(T, L) -> proplists:get_value(T,L).
gv(T, L, D) -> proplists:get_value(T,L, D).
%% sv(K, V, L) -> lists:keystore(K, 1, L, {K, V}).
%% dv(K, L) -> lists:keydelete(K, 1, L).

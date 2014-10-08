%% Copyright (c) 2011-2012 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.

%% @doc A error_logger backend for redirecting events into lager.
%% Error messages and crash logs are also optionally written to a crash log.

%% @see lager_crash_log

%% @private

-module(error_logger_watchdog_h).

-include_lib("lager/include/lager.hrl").

-behaviour(gen_event).

-export([init/1, handle_call/2, handle_event/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {
          %% how many messages per second we try to deliver
          hwm = undefined :: 'undefined' | pos_integer(),
          %% how many messages we've received this second
          mps = 0 :: non_neg_integer(),
          %% the current second
          lasttime = os:timestamp() :: erlang:timestamp(),
          %% count of dropped messages this second
          dropped = 0 :: non_neg_integer()
         }).

-define(LOGMSG(Level, Pid, Msg),
        case ?SHOULD_LOG(Level) of
            true ->
                _ =lager:log(Level, Pid, Msg),
                ok;
            _ -> ok
        end).

-define(LOGFMT(Level, Pid, Fmt, Args),
        case ?SHOULD_LOG(Level) of
            true ->
                _ = lager:log(Level, Pid, Fmt, Args),
                ok;
            _ -> ok
        end).

-define(CRASH_LOG(Event),
        lager_watchdog_srv:log(Event)).

-spec init(any()) -> {ok, #state{}}.
init([HighWaterMark]) ->
    {ok, #state{hwm=HighWaterMark}}.

handle_call(_Request, State) ->
    {ok, unknown_call, State}.

handle_event(Event, State) ->
    case check_hwm(State) of
        {true, NewState} ->
            log_event(Event, NewState);
        {false, NewState} ->
            {ok, NewState}
    end.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% internal functions

check_hwm(State = #state{hwm = undefined}) ->
    {true, State};
check_hwm(State = #state{mps = Mps, hwm = Hwm}) when Mps < Hwm ->
    %% haven't hit high water mark yet, just log it
    {true, State#state{mps=Mps+1}};
check_hwm(State = #state{hwm = Hwm, lasttime = Last, dropped = Drop}) ->
    %% are we still in the same second?
    {M, S, _} = Now = os:timestamp(),
    case Last of
        {M, S, _} ->
            %% still in same second, but have exceeded the high water mark
            NewDrops = discard_messages(Now, 0),
            {false, State#state{dropped=Drop+NewDrops}};
        _ ->
            %% different second, reset all counters and allow it
            case Drop > 0 of
                true ->
                    ?LOGFMT(warning, self(), "lager_error_logger_h dropped ~p messages in the last second that exceeded the limit of ~p messages/sec",
                            [Drop, Hwm]);
                false ->
                    ok
            end,
            {true, State#state{dropped = 0, mps=1, lasttime = Now}}
    end.

discard_messages(Second, Count) ->
    {M, S, _} = os:timestamp(),
    case Second of
        {M, S, _} ->
            receive
                %% we only discard gen_event notifications, because
                %% otherwise we might discard gen_event internal
                %% messages, such as trapped EXITs
                {notify, _Event} ->
                    discard_messages(Second, Count+1);
                {_From, _Tag, {sync_notify, _Event}} ->
                    discard_messages(Second, Count+1)
            after 0 ->
                    Count
            end;
        _ ->
            Count
    end.

log_event(Event, State) ->
    case Event of
        {error, _GL, {_Pid, _Fmt, Args}} ->
            ?CRASH_LOG({error, Args});
        {error_report, _GL, {_Pid, std_error, D}} ->
            ?CRASH_LOG({error_report1, D});
        {error_report, _GL, {_Pid, supervisor_report, D}} ->
            ?CRASH_LOG({error_report2, D});
        {error_report, _GL, {_Pid, crash_report, [Self, Neighbours]}} ->
            ?CRASH_LOG({error_report3, Self, Neighbours});
        %% {warning_msg, _GL, {Pid, Fmt, Args}} ->
        %%     ?LOGMSG(warning, Pid, lager:safe_format(Fmt, Args, ?DEFAULT_TRUNCATION));
        %% {warning_report, _GL, {Pid, std_warning, Report}} ->
        %%     ?LOGMSG(warning, Pid, print_silly_list(Report));
        %% {info_msg, _GL, {Pid, Fmt, Args}} ->
        %%     ?LOGMSG(info, Pid, lager:safe_format(Fmt, Args, ?DEFAULT_TRUNCATION));
        %% {info_report, _GL, {Pid, std_info, D}} when is_list(D) ->
        %%     Details = lists:sort(D),
        %%     case Details of
        %%         [{application, App}, {exited, Reason}, {type, _Type}] ->
        %%             ?LOGFMT(info, Pid, "Application ~w exited with reason: ~s",
        %%                     [App, format_reason(Reason)]);
        %%         _ ->
        %%             ?LOGMSG(info, Pid, print_silly_list(D))
        %%     end;
        %% {info_report, _GL, {Pid, std_info, D}} ->
        %%     ?LOGFMT(info, Pid, "~w", [D]);
        %% {info_report, _GL, {P, progress, D}} ->
        %%     Details = lists:sort(D),
        %%     case Details of
        %%         [{application, App}, {started_at, Node}] ->
        %%             ?LOGFMT(info, P, "Application ~w started on node ~w",
        %%                     [App, Node]);
        %%         [{started, Started}, {supervisor, Name}] ->
        %%             MFA = format_mfa(get_value(mfargs, Started)),
        %%             Pid = get_value(pid, Started),
        %%             ?LOGFMT(debug, P, "Supervisor ~w started ~s at pid ~w",
        %%                     [supervisor_name(Name), MFA, Pid]);
        %%         _ ->
        %%             ?LOGMSG(info, P, "PROGRESS REPORT " ++ print_silly_list(D))
        %%     end;
        _ ->
            ok
    end,
    {ok, State}.


%% format_reason({'function not exported', [{M, F, A},MFA|_]}) ->
%%     ["call to undefined function ", format_mfa({M, F, length(A)}),
%%      " from ", format_mfa(MFA)];
%% format_reason({'function not exported', [{M, F, A, _Props},MFA|_]}) ->
%%     %% R15 line numbers
%%     ["call to undefined function ", format_mfa({M, F, length(A)}),
%%      " from ", format_mfa(MFA)];
%% format_reason({undef, [MFA|_]}) ->
%%     ["call to undefined function ", format_mfa(MFA)];
%% format_reason({bad_return, {_MFA, {'EXIT', Reason}}}) ->
%%     format_reason(Reason);
%% format_reason({bad_return, {MFA, Val}}) ->
%%     ["bad return value ", print_val(Val), " from ", format_mfa(MFA)];
%% format_reason({bad_return_value, Val}) ->
%%     ["bad return value: ", print_val(Val)];
%% format_reason({{bad_return_value, Val}, MFA}) ->
%%     ["bad return value: ", print_val(Val), " in ", format_mfa(MFA)];
%% format_reason({{badrecord, Record}, [MFA|_]}) ->
%%     ["bad record ", print_val(Record), " in ", format_mfa(MFA)];
%% format_reason({{case_clause, Val}, [MFA|_]}) ->
%%     ["no case clause matching ", print_val(Val), " in ", format_mfa(MFA)];
%% format_reason({function_clause, [MFA|_]}) ->
%%     ["no function clause matching ", format_mfa(MFA)];
%% format_reason({if_clause, [MFA|_]}) ->
%%     ["no true branch found while evaluating if expression in ", format_mfa(MFA)];
%% format_reason({{try_clause, Val}, [MFA|_]}) ->
%%     ["no try clause matching ", print_val(Val), " in ", format_mfa(MFA)];
%% format_reason({badarith, [MFA|_]}) ->
%%     ["bad arithmetic expression in ", format_mfa(MFA)];
%% format_reason({{badmatch, Val}, [MFA|_]}) ->
%%     ["no match of right hand value ", print_val(Val), " in ", format_mfa(MFA)];
%% format_reason({emfile, _Trace}) ->
%%     "maximum number of file descriptors exhausted, check ulimit -n";
%% format_reason({system_limit, [{M, F, _}|_] = Trace}) ->
%%     Limit = case {M, F} of
%%                 {erlang, open_port} ->
%%                     "maximum number of ports exceeded";
%%                 {erlang, spawn} ->
%%                     "maximum number of processes exceeded";
%%                 {erlang, spawn_opt} ->
%%                     "maximum number of processes exceeded";
%%                 {erlang, list_to_atom} ->
%%                     "tried to create an atom larger than 255, or maximum atom count exceeded";
%%                 {ets, new} ->
%%                     "maximum number of ETS tables exceeded";
%%                 _ ->
%%                     {Str, _} = lager_trunc_io:print(Trace, 500),
%%                     Str
%%             end,
%%     ["system limit: ", Limit];
%% format_reason({badarg, [MFA,MFA2|_]}) ->
%%     case MFA of
%%         {_M, _F, A, _Props} when is_list(A) ->
%%             %% R15 line numbers
%%             ["bad argument in call to ", format_mfa(MFA), " in ", format_mfa(MFA2)];
%%         {_M, _F, A} when is_list(A) ->
%%             ["bad argument in call to ", format_mfa(MFA), " in ", format_mfa(MFA2)];
%%         _ ->
%%             %% seems to be generated by a bad call to a BIF
%%             ["bad argument in ", format_mfa(MFA)]
%%     end;
%% format_reason({{badarity, {Fun, Args}}, [MFA|_]}) ->
%%     {arity, Arity} = lists:keyfind(arity, 1, erlang:fun_info(Fun)),
%%     [io_lib:format("fun called with wrong arity of ~w instead of ~w in ",
%%                    [length(Args), Arity]), format_mfa(MFA)];
%% format_reason({noproc, MFA}) ->
%%     ["no such process or port in call to ", format_mfa(MFA)];
%% format_reason({{badfun, Term}, [MFA|_]}) ->
%%     ["bad function ", print_val(Term), " in ", format_mfa(MFA)];
%% format_reason({Reason, [{M, F, A}|_]}) when is_atom(M), is_atom(F), is_integer(A) ->
%%     [format_reason(Reason), " in ", format_mfa({M, F, A})];
%% format_reason({Reason, [{M, F, A, Props}|_]}) when is_atom(M), is_atom(F), is_integer(A), is_list(Props) ->
%%     %% line numbers
%%     [format_reason(Reason), " in ", format_mfa({M, F, A, Props})];
%% format_reason(Reason) ->
%%     {Str, _} = lager_trunc_io:print(Reason, 500),
%%     Str.

%% format_mfa({M, F, A}) when is_list(A) ->
%%     {FmtStr, Args} = format_args(A, [], []),
%%     io_lib:format("~w:~w("++FmtStr++")", [M, F | Args]);
%% format_mfa({M, F, A}) when is_integer(A) ->
%%     io_lib:format("~w:~w/~w", [M, F, A]);
%% format_mfa({M, F, A, Props}) when is_list(Props) ->
%%     case get_value(line, Props) of
%%         undefined ->
%%             format_mfa({M, F, A});
%%         Line ->
%%             [format_mfa({M, F, A}), io_lib:format(" line ~w", [Line])]
%%     end;
%% format_mfa([{M, F, A}, _]) ->
%%     %% this kind of weird stacktrace can be generated by a uncaught throw in a gen_server
%%     format_mfa({M, F, A});
%% format_mfa([{M, F, A, Props}, _]) when is_list(Props) ->
%%     %% this kind of weird stacktrace can be generated by a uncaught throw in a gen_server
%%     format_mfa({M, F, A, Props});
%% format_mfa(Other) ->
%%     io_lib:format("~w", [Other]).

%% format_args([], FmtAcc, ArgsAcc) ->
%%     {string:join(lists:reverse(FmtAcc), ", "), lists:reverse(ArgsAcc)};
%% format_args([H|T], FmtAcc, ArgsAcc) ->
%%     {Str, _} = lager_trunc_io:print(H, 100),
%%     format_args(T, ["~s"|FmtAcc], [Str|ArgsAcc]).


%% @doc Faster than proplists, but with the same API as long as you don't need to
%% handle bare atom keys
%% get_value(Key, Value) ->
%%     get_value(Key, Value, undefined).

%% get_value(Key, List, Default) ->
%%     case lists:keyfind(Key, 1, List) of
%%         false -> Default;
%%         {Key, Value} -> Value
%%     end.

%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@schroedinger.local>
%%% @copyright (C) 2014, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created :  8 Oct 2014 by Heinz Nikolaus Gies <heinz@schroedinger.local>
%%%-------------------------------------------------------------------
-module(lager_watchdog_srv).

-behaviour(gen_server).

%% API
-export([start_link/0, log/1, log/2, set_version/1]).
-export([raise/3, clear/2]).

-ignore_xref([start_link/0, set_version/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(PING_TIME, 10000). %% This is 10s
-record(state, {servers, badservers = [], socket, id, version = <<"unknown">>}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


log(Msg) ->
    gen_server:cast(?SERVER, {log, Msg}).

log(Src, Msg) ->
    gen_server:cast(?SERVER, {log, Src, Msg}).

set_version(Vsn) when is_binary(Vsn) ->
    gen_server:cast(?SERVER, {vsn, Vsn}).

raise(Type, Alert, Severity) ->
    gen_server:cast(?SERVER, {raise, Type, Alert, Severity}).

clear(Type, Alert) ->
    gen_server:cast(?SERVER, {clear, Type, Alert}).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    case gen_event:call(lager_event, lager_watchdog, get_servers) of
        {error, bad_module} ->
            {ok, #state{socket = inactive}};
        [{Addr, Port} | Srvs] ->
            ID = gen_event:call(lager_event, lager_watchdog, get_id),
            S0 = #state{servers = Srvs, badservers = [{Addr, Port}]},
            erlang:send_after(?PING_TIME, self(), ping),
            case gen_tcp:connect(Addr, Port, [binary, {packet, 4}]) of
                {ok, Sock} ->
                    {ok, S0#state{socket = Sock, id=ID}};
                _ ->
                    {ok, S0}
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_, State = #state{socket = inactive}) ->
    {noreply, State};

handle_cast({vsn, Vsn}, State) ->
    {noreply, State#state{version = Vsn}};

handle_cast({log, Msg}, State) ->
    {noreply, send(Msg, State)};

handle_cast({log, Src, Msg}, State) ->
    {noreply, send({msg, Msg, Src}, State)};

handle_cast({raise, Type, Alert, Severity}, State) ->
    {noreply, send({raise, Type, Alert, Severity}, State)};

handle_cast({clear, Type, Alert}, State) ->
    {noreply, send({clear, Type, Alert}, State)};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(ping, State) ->
    erlang:send_after(?PING_TIME, self(), ping),
    {noreply, send(ping, State)};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

send(M, State = #state{servers = [], badservers = Ss}) ->
    send(M, State#state{servers = lists:reverse(Ss), badservers = []});

send(M, State = #state{socket = undefined, servers = [{Addr, Port} | R],
                       badservers = Ss}) ->
    case gen_tcp:connect(Addr, Port, [binary, {packet, 4}], 100) of
        {ok, Sock} ->
            send(M, State#state{socket = Sock, servers = R,
                                badservers=[{Addr, Port} | Ss]});
        _ ->
            State
    end;

send(Raw, State = #state{socket = Sock, id=ID, version = Vsn}) ->
    case prettyfy_msg(Raw) of
        {ok, M} ->
            case gen_tcp:send(Sock, term_to_binary({ID, Vsn, M})) of
                ok ->
                    State;
                _ ->
                    State#state{socket = undefined}
            end;
        _ ->
            State
    end.
prettyfy_msg(ping) ->
    {ok, ping};

prettyfy_msg({raise, Type, Alert, Severity}) ->
    {ok, {raise, Type, Alert, Severity}};

prettyfy_msg({clear, Type, Alert}) ->
    {ok, {clear, Type, Alert}};

prettyfy_msg({msg, {lager, Svrt}, Src}) ->
    {ok, {{lager, Svrt}, Src}};

prettyfy_msg({msg, Msg, Src}) ->
    {ok, {msg, Msg, Src}};

prettyfy_msg({flm, File, Line, Msg}) when is_list(File) ->
    {ok, {msg, Msg, {fl, {list_to_binary(File), Line}}}};

prettyfy_msg({flm, File, Line, Msg}) ->
    {ok, {msg, Msg, {fl, {File, Line}}}};

prettyfy_msg({error, [_Pid, _Sig, _State, Cause]}) ->
    prettify_cause(Cause);

prettyfy_msg({error_report2, D}) ->
    case prettify_cause(get_value(reason, D)) of
        {ok, R} ->
            {ok, R};
        _ ->
            %% {error_report2,
            %%  [{supervisor,{<10225.190.0>,poolboy_sup}},
            %%   {errorContext,shutdown_error},
            %%   {reason,shutdown},
            %%   {offender,
            %%    [{nb_children,3},
            %%     {name,fifo_s3_download_worker},
            %%     {mfargs,{fifo_s3_download_worker,start_link,[[]]}},
            %%     {restart_type,temporary},
            %%     {shutdown,5000},
            %%     {child_type,worker}]}]}
            no_log
    end;

prettyfy_msg({error_report3, M, _}) ->
    case get_value(error_info, M) of
        undefined ->
            lager:warning("[error_report3] No error_info for: ~p", M),
            no_log;
        Info ->
            case prettify_info(Info) of
                {ok, R} ->
                    {ok, R};
                _ ->
                    no_log
            end
    end;

prettyfy_msg(M) ->
    lager:warning("[no_log] can't do anything with: ~p", M),
    no_log.


%% Location:
%% [{gen_fsm,terminate,7,[{file,"gen_fsm.erl"},{line,600}]},
%%   {proc_lib,init_p_do_apply,3,
%%    [{file,"proc_lib.erl"},{line,239}]}]

%% Cause:
%% {
%%   {{badrecord,door},
%%    [{ezdoor_server,handle_call,3,
%%      [{file,"src/ezdoor_server.erl"},{line,113}]},
%%     {gen_server,handle_msg,5,
%%      [{file,"gen_server.erl"},{line,585}]},
%%     {proc_lib,init_p_do_apply,3,
%%      [{file,"proc_lib.erl"},{line,239}]}]},
%%   {gen_server,call,
%%    [ezdoor_server,{remove,#Ref<10225.0.0.1100>}]}
%% }

%% {exit,
%%  {noproc,
%%   {gen_server,call,
%%    [pooler_sup,
%%     {terminate_child,
%%      'pooler_snarl@192.168.221.201:4200_pool_sup'},
%%     infinity]}},
%%  [{gen_server,terminate,6,
%%    [{file,"gen_server.erl"},{line,722}]},
%%   {proc_lib,init_p_do_apply,3,
%%    [{file,"proc_lib.erl"},{line,239}]}]}


%% Call: 
%% {gen_server,call,
%%  [pooler_sup,
%%   {terminate_child,
%%    'pooler_snarl@192.168.221.201:4200_pool_sup'},
%%   infinity]}
prettify_info({exit, Cause, Location}) ->
    case prettify_cause(Cause) of
        {ok, R} ->
            {ok, R};
        _ ->
            case Location of
                [MFAF | _] ->
                    {ok, {exit, mfaf(MFAF)}};
                _ ->
                    no_log
            end
    end;

prettify_info(I) ->
    lager:warning("unknown info: ~p", [I]),
    no_log.

%% Stack: 
%% [{ezdoor_server,handle_call,3,
%%      [{file,"src/ezdoor_server.erl"},{line,113}]},
%%     {gen_server,handle_msg,5,
%%      [{file,"gen_server.erl"},{line,585}]},
%%     {proc_lib,init_p_do_apply,3,
%%      [{file,"proc_lib.erl"},{line,239}]}]

prettify_cause({{Reason, [MFAF | _]}, _}) ->
    {ok, {Reason, mfaf(MFAF)}};

prettify_cause({noproc, Src = {_M, _F, [P | _]}}) ->
    {ok, {{noproc, P}, prettify_src(Src)}};

prettify_cause({{badmatch, _}, [MFAF | _]}) ->
    {ok, {badmatch, mfaf(MFAF)}};

prettify_cause({Reason, Src}) when is_atom(Reason) ->
    {ok, {Reason, prettify_src(Src)}};

prettify_cause({Data, _}) when is_list(Data)->
    Reason = get_value(reason, Data),
    Stack = get_value(stacktrace, Data),
    prettify_cause({Reason, Stack});

prettify_cause(S) ->
    lager:warning("unknown cause: ~p", [S]),
    no_log.

prettify_src({M, F, A}) ->
    {mfa, {a2b(M), a2b(F), length(A)}}.

mfaf({M, F, A, D}) ->
    File = get_value(file, D),
    Line = get_value(line, D),
    {mfaf, {a2b(M), a2b(F), A, {l2b(File), Line}}}.


get_value(Key, List) ->
    get_value(Key, List, undefined).

get_value(Key, List, Default) ->
    case lists:keyfind(Key, 1, List) of
        false -> Default;
        {Key, Value} -> Value
    end.


a2b(A) ->
    l2b(atom_to_list(A)).

l2b(L) ->
    list_to_binary(L).

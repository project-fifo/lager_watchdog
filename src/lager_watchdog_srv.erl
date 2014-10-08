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
-export([start_link/0, log/1, log/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {servers, socket}).

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

log(F, L, Msg) ->
    gen_server:cast(?SERVER, {log, F, L, Msg}).

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
    {Addr, Port} = gen_event:call(lager_event, lager_watchdog, get_servers),
    S0 = #state{servers = [{Addr, Port}]},
    case gen_tcp:connect(Addr, Port, [binary, {packet, 4}]) of
        {ok, Sock} ->
            {ok, S0#state{socket = Sock}};
        _ ->
            {ok, S0}
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
handle_cast({log, Msg}, State) ->
    {noreply, send(Msg, State)};

handle_cast({log, File, Line, Msg}, State) ->
    {noreply, send({File, Line, Msg}, State)};

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

send(M, State = #state{socket = undefined, servers = [{Addr, Port} | _]}) ->
    case gen_tcp:connect(Addr, Port, [binary, {packet, 4}], 100) of
        {ok, Sock} ->
            send(M, State#state{socket = Sock});
        _ ->
            State
    end;

send(M, State = #state{socket = Sock}) ->
    case gen_tcp:send(Sock, term_to_binary(M)) of
        ok ->
            State;
        _ ->
            State#state{socket = undefined}
    end.


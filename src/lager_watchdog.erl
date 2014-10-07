-module(lager_watchdog).

-behaviour(gen_event).

-export([init/1, handle_call/2, handle_event/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {
          id,
          cluster_id,
          system,
          level,
          prefix,
          servers,
          bad_servers = [],
          socket
         }).

-include_lib("lager/include/lager.hrl").

%% @private
init([ClusterID, System, Level, Servers]) ->
    State = #state{
               id = {?MODULE, {ClusterID, System}},
               level = lager_util:config_to_mask(Level),
               cluster_id = ClusterID,
               system = System,
               prefix = {ClusterID, System},
               servers = Servers},
    {ok, State}.


%% @private
handle_call(get_loglevel, #state{level=Level} = State) ->
    {ok, Level, State};

handle_call({set_loglevel, Level}, State) ->
    try lager_util:config_to_mask(Level) of
        Lvl ->
            {ok, ok, State#state{level=Lvl}}
    catch
        _:_ ->
            {ok, {error, bad_log_level}, State}
    end;

handle_call(_Request, State) ->
    {ok, ok, State}.

%% @private
handle_event({log, Level, {Date, Time}, [LevelStr, Location, Message]},
             #state{level=LogLevel} = State) when Level =< LogLevel ->
    io:format("Log1: ~p~n", [{log, Level, {Date, Time}, [LevelStr, Location, Message]}]),
    {ok, State};

handle_event({log, Message}, #state{level=Level} = State) ->
    case lager_util:is_loggable(Message, Level, State#state.id) of
        true ->
            io:format("Log2: ~p~n", [{log, Message}]),
            {ok, State};
        false ->
            {ok, State}
    end;

handle_event(_Event, State) ->
    {ok, State}.

%% @private
handle_info(_Info, State) ->
    {ok, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

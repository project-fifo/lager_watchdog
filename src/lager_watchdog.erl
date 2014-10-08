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
          servers
         }).

-include_lib("lager/include/lager.hrl").

%% @private
init([ClusterID, System, Level, Servers]) ->
    C = list_to_binary(ClusterID),
    S = list_to_binary(System),
    N = list_to_binary(atom_to_list(node())),
    State = #state{
               id = {?MODULE, {ClusterID, System}},
               level = lager_util:config_to_mask(Level),
               cluster_id = C,
               system = S,
               prefix = {C, S, N},
               servers = Servers},
    {ok, State}.


%% @private
handle_call(get_loglevel, #state{level=Level} = State) ->
    {ok, Level, State};

handle_call(get_servers, #state{servers=Servers} = State) ->
    {ok, Servers, State};

handle_call(get_id, #state{prefix = ID} = State) ->
    {ok, ID, State};

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
    lager_watchdog_srv:log([{log, Level, {Date, Time}, [LevelStr, Location, Message]}]),
    {ok, State};

handle_event({log, Message}, #state{level=Level} = State) ->
    case lager_util:is_loggable(Message, Level, State#state.id) of
        true ->
            M = lager_msg:metadata(Message),
            File = case {v(file, M), v(module, M)} of
                       {undefined, undefined} -> undefined;
                       {undefined, Mod} -> list_to_binary(atom_to_list(Mod)
                                                          ++ ".erl");
                       {F, _} -> list_to_binary(F)
                   end,
            Msg = {lager, lager_msg:severity(Message)},
            case {File, v(line, M), v(function, M)} of
                {undefined, _, _} -> ok;
                {_, undefined, _} -> ok;
                {File1, Line, undefined} ->
                    lager_watchdog_srv:log({fl, File1, Line}, Msg);
                {File1, Line, Func} ->
                    lager_watchdog_srv:log({flf, File1, Line, Func}, Msg)
            end,
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

v(Key, List) ->
    v(Key, List, undefined).

v(Key, List, Default) ->
    case lists:keyfind(Key, 1, List) of
        false -> Default;
        {Key, Value} -> Value
    end.

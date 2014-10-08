-module(lager_watchdog_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Reply = lager_watchdog_sup:start_link(),
    HighWaterMark = case application:get_env(lager, error_logger_hwm) of
                        {ok, undefined} ->
                            undefined;
                        {ok, HwmVal} when is_integer(HwmVal), HwmVal > 0 ->
                            HwmVal;
                        {ok, BadVal} ->
                            _ = lager:log(
                                  warning, self(),
                                  "Invalid error_logger high water mark: ~p, "
                                  "disabling", [BadVal]),
                            undefined;
                        undefined ->
                            undefined
                    end,
    supervisor:start_child(
      lager_handler_watcher_sup,
      [error_logger, error_logger_watchdog_h, [HighWaterMark]]),
    Reply.

stop(_State) ->
    ok.

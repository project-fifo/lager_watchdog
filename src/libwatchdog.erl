%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@schroedinger.local>
%%% @copyright (C) 2014, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created : 27 Oct 2014 by Heinz Nikolaus Gies <heinz@schroedinger.local>
%%%-------------------------------------------------------------------
-module(libwatchdog).

-export([raise/3, clear/2]).
-ignore_xref([raise/3, clear/2]).

raise(Type, Alert, Severity) ->
    lager_watchdog_srv:raise(Type, Alert, Severity).

clear(Type, Alert) ->
    lager_watchdog_srv:clear(Type, Alert).


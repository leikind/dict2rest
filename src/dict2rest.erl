%% @author Yuri Leikind
%% @copyright 2013 Yuri Leikind.

%% @doc dict2rest startup code

-module(dict2rest).
-author('Yuri Leikind <yuri.leikind@gmail.com>').
-export([start/0, start_link/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    ensure_started(inets),
    ensure_started(crypto),
    ensure_started(mochiweb),
    application:set_env(webmachine, webmachine_logger_module,
                        webmachine_logger),
    ensure_started(webmachine),
    dict2rest_sup:start_link().

%% @spec start() -> ok
%% @doc Start the dict2rest server.
start() ->
    ensure_started(inets),
    ensure_started(crypto),
    ensure_started(mochiweb),
    application:set_env(webmachine, webmachine_logger_module,
                        webmachine_logger),
    ensure_started(webmachine),
    application:start(dict2rest).

%% @spec stop() -> ok
%% @doc Stop the dict2rest server.
stop() ->
    Res = application:stop(dict2rest),
    application:stop(webmachine),
    application:stop(mochiweb),
    application:stop(crypto),
    application:stop(inets),
    Res.

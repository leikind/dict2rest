%% @author Yuri Leikind
%% @copyright 2013 Yuri Leikind.

%% @doc Callbacks for the dict2rest application.


-module(dict2rest_app).
-author('Yuri Leikind <yuri.leikind@gmail.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for dict2rest.
start(_Type, _StartArgs) ->
    dict2rest_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for dict2rest.
stop(_State) ->
    ok.

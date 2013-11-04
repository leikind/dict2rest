%% @author Yuri Leikind
%% @copyright 2013 Yuri Leikind.

%% @doc one_for_one supervisor for dict2rest_client_worker

-module(dict2rest_client_worker_supervisor).

-behaviour(supervisor).


%% API
-export([start_link/0]).


%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).


start_link() ->
    supervisor:start_link(
      {local, ?SERVER}, % register the supervisor under this name locally
      ?MODULE,          % module name where the supervisor code is
      []                % arguments
    ).

init([]) ->

  {ok, Host} = application:get_env(dict2rest, dictd_host),
  {ok, Port} = application:get_env(dict2rest, dictd_port),

  Server = {                                     % yes, I am an OTP newbie :)
    dict2rest_client_worker,                     % term that the supervisor uses to identify the specification internally
    {                                            % how to start the process
      dict2rest_client_worker,
      start_link,
      [Host, Port]
    },
    permanent,                                   % a long-lived service that should always be restarted if it terminates for any reason
                                                 % temporary: processes that should never be restarted
                                                 % transient:  processes that should be restarted only if they terminate abnormally but not upon normal termination
    2000,                                        % milliseconds for a soft shutdown
    worker,                                      % this is a worker
    [dict2rest_client_worker]                    % modules that the process depends on
  },

  Children = [Server],

  RestartStrategy = {one_for_one, 0, 1},

{ok, {RestartStrategy, Children}}.


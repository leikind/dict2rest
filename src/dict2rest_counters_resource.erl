%% @author Yuri Leikind
%% @copyright 2013 Yuri Leikind.


-module(dict2rest_counters_resource).
-export([init/1, to_json/2, allowed_methods/2, content_types_provided/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.

allowed_methods(ReqData, State) -> {['GET'], ReqData, State}.

content_types_provided(ReqData, State) ->
    {[{"application/json", to_json}], ReqData, State}.


to_json(ReqData, State) ->

    MatchCounter  = dict2rest_client_worker:match_counter(),
    DefineCounter = dict2rest_client_worker:define_counter(),

    {
      dict2rest_output:struct([{"matches performed", MatchCounter}, {"defines performed", DefineCounter}]),
      ReqData,
      State
    }.

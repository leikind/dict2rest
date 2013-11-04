%% @author Yuri Leikind
%% @copyright 2013 Yuri Leikind.

%% @doc
%%   Webmachine resource for obtaining a list of strategies.
%%
%%   /dictionaries
%%
%% @end

-module(dict2rest_dictionaries_resource).
-export([init/1, to_json/2, allowed_methods/2, content_types_provided/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.

allowed_methods(ReqData, State) ->  {['GET'], ReqData, State}.

content_types_provided(ReqData, State) ->
    {[{"application/json", to_json}], ReqData, State}.

to_json(ReqData, State) ->

    Dictionaries = dict2rest_client_worker:dictionaries(),

    {dict2rest_output:struct(Dictionaries), ReqData, State}.

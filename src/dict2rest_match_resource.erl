%% @author Yuri Leikind
%% @copyright 2013 Yuri Leikind.

%% @doc
%%   Webmachine resource for matching words.
%%
%%   /match?w=WORD
%%
%%   /match/STRATEGY?w=WORD
%% @end


-module(dict2rest_match_resource).
-export([init/1, to_json/2, allowed_methods/2, content_types_provided/2, resource_exists/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include("include/common_definitions.hrl").

init([]) -> {ok, undefined}.

allowed_methods(ReqData, State) ->  {['GET'], ReqData, State}.

content_types_provided(ReqData, State) ->
    {[{"application/json", to_json}], ReqData, State}.


resource_exists(ReqData, State) ->

  {Response, NewState} = case wrq:path_info(strategy, ReqData) of
    undefined    -> {true, {strategy_and_dictionary, ?ALL_STRATEGIES, ?ALL_DICTIONARIES}};
    Strategy   ->

      Strategies = dict2rest_client_worker:strategies(),

      case proplists:is_defined(unicode:characters_to_binary(Strategy), Strategies) of

        true  ->
          case wrq:path_info(dictionary, ReqData) of
            undefined  -> {true, {strategy_and_dictionary, Strategy, ?ALL_DICTIONARIES}};
            Dictionary ->

              Dictionaries = dict2rest_client_worker:dictionaries(),

              case proplists:is_defined(unicode:characters_to_binary(Dictionary), Dictionaries) of
                true  ->  {true, {strategy_and_dictionary, Strategy, Dictionary}};
                false ->  {false, State}
              end
          end;

        false ->  {false, State}
      end

  end,

  {Response, ReqData, NewState}.


to_json(ReqData, State) ->
  case wrq:get_qs_value("w", ReqData) of
    %  TO DO: give the right HTTP response code
    undefined -> { mochijson:encode({struct, [{error, no_word}]}), ReqData, State };

    Word  ->
      {strategy_and_dictionary, Strategy, Dictionary} = State,

      Matches = dict2rest_client_worker:match(list_to_binary(Word), Strategy, Dictionary),

      {dict2rest_output:struct(Matches), ReqData, State}
  end.

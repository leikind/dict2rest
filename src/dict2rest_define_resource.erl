%% @author Yuri Leikind
%% @copyright 2013 Yuri Leikind.

%% @doc
%%   Webmachine resource for getting word definitions.
%%
%%   /define?w=WORD
%%
%%   /define/DICTIONARY?w=WORD
%% @end

-module(dict2rest_define_resource).
-export([init/1, to_json/2, allowed_methods/2, content_types_provided/2, resource_exists/2]).
% -export([init/1, to_json/2, allowed_methods/2, content_types_provided/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include("include/common_definitions.hrl").

init([]) -> {ok, undefined}.

allowed_methods(ReqData, State) ->  {['GET'], ReqData, State}.

content_types_provided(ReqData, State) ->
    {[{"application/json", to_json}], ReqData, State}.


resource_exists(ReqData, State) ->

  {Response, NewState} = case wrq:path_info(dictionary, ReqData) of
    undefined    -> {true, {dictionary, "*"}};
    Dictionary   ->

      Dictionaries = dict2rest_client_worker:dictionaries(),

      case proplists:is_defined(unicode:characters_to_binary(Dictionary), Dictionaries) of
        true  ->  {true, {dictionary, Dictionary}};
        false ->  {false, State}
      end
  end,

  {Response, ReqData, NewState}.

to_json(ReqData, State) ->

  case wrq:get_qs_value("w", ReqData) of
    %  TO DO: give the right HTTP response code???
    undefined -> { mochijson:encode({struct, [{error, no_word}]}), ReqData, State };

    Word  ->
      {dictionary, Dictionary} = State,

      Definitions = dict2rest_client_worker:define(list_to_binary(Word), Dictionary),

      Struct = case Definitions of
        {no_such_dictionary, NotFoundDictionary} ->
          Message = unicode:characters_to_binary(["no such dictionary: ", NotFoundDictionary]),
          [[{"error",  Message}]];
        _ ->  lists:map(
                fun(Definition) ->
                  [
                    {word,                   Definition#word_definition.word},
                    {dictionary,             Definition#word_definition.dictionary},
                    {dictionary_description, Definition#word_definition.dictionary_description},
                    {definition,             Definition#word_definition.definition}
                  ]
                end,
                Definitions)
      end,

      {dict2rest_output:array(Struct), ReqData, State}
  end.

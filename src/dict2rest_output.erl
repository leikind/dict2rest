%% @author Yuri Leikind
%% @copyright 2013 Yuri Leikind.

%% @doc Functions common for webmachine resources

-module(dict2rest_output).

-export([struct/1, array/1]).

%% @doc Create a mochijson2 encoder configured for UTF-8
-spec encoder() -> function().
encoder()-> mochijson2:encoder([{utf8, true}]).

%% @doc Encode a proplist as JSON
-spec struct([{binary(), binary()}]) -> binary().
struct(Out) ->
  Encoder = encoder(),
  Encoder({struct, Out}).

%% @doc Encode a list of tuples as JSON
-spec array([tuple()]) -> binary().
array(Out) ->
  Encoder = encoder(),
  Encoder({array, Out}).

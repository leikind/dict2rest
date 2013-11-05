
-module(dict2rest_home_resource).
-export([init/1, to_text/2, allowed_methods/2, content_types_provided/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include("include/common_definitions.hrl").

init([]) -> {ok, undefined}.

allowed_methods(ReqData, State) ->  {['GET'], ReqData, State}.

content_types_provided(ReqData, State) ->
    {[{"text/plain", to_text}], ReqData, State}.

to_text(ReqData, State) ->

  Text  = <<"DICT is a dictionary network protocol created by the DICT Development Group.[1] It is described by RFC 2229, published in 1997.

This application embeds a DICT client and allows you to query DICT servers via a simple RESTful JSON webservice:


A list of dictionaries available on the DICT server:

        /dictionaries


A list of matching strategies supported by the DICT server:

        /strategies


Find definitions of a word in all dictionaries:

        /define?w=WORD


Find definitions of a word in a certain dictionary:

        /define/DICTIONARY?w=WORD

For example:

        /define/muiswerk?w=meisje


Match a word using all strategies:

        /match?w=WORD

Match a word using a certain strategy:

        /match/STRATEGY?w=apple


For example

        /match/prefix?w=appl


2013 Yuri Leikind
">>,
  {Text, ReqData, State}.

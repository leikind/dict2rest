%% @author Yuri Leikind
%% @copyright 2013 Yuri Leikind
%% @doc
%%   RFC 2229 client.
%%   The Dictionary Server Protocol (DICT) is an old TCP transaction based query/response protocol that
%%   allows a client to access dictionary definitions from a set of natural language dictionary databases.
%%
%%   See RFC 2229 for details. http://tools.ietf.org/html/rfc2229
%% @end

-module(dict2rest_dict_client).

-include("include/common_definitions.hrl").

-define(CLIENT_NAME, <<"client github.com/leikind/dict2rest">>).
-define(DEFAULT_HOST, "dict.mova.org").
-define(DEFAULT_PORT, 2628).
-define(EOL, "\r\n").
-define(EOD, <<".\r\n">>).
-define(RESPONSE_OK, 250).


-record(client, {
  sock   :: socket(),
  host   :: string(),
  port   :: pos_integer(),
  banner :: binary()
}).

-type word_definition() :: #word_definition{}.
-type client() :: #client{}.
-type socket() :: port().

-export_type([word_definition/0, client/0]).

-export([
         connect/0, connect/2,
         disconnect/1,
         server_banner/1,
         databases/1,
         strategies/1,
         server_help/1,
         server_info/1,
         define/2, define/3,
         match/2, match/3, match/4
]).

%% @doc Connect to the default dictd server.
-spec connect() -> client().
connect() -> connect(?DEFAULT_HOST, ?DEFAULT_PORT).

%% @doc Connect to a dictd server.
-spec connect(nonempty_string(), pos_integer()) -> client().
connect(Host, Port) ->
  case
    gen_tcp:connect(Host, Port, [binary, {active, false}, {packet, line}, {buffer, 400}]) of
      {ok, Socket}    ->

        BannerLine = readline(Socket),
        % io:format("~ts~n", [BannerLine]),

        send_command(Socket, ?CLIENT_NAME),
        Response = readline(Socket),

        case extract_reply_code(Response) of
          ?RESPONSE_OK ->
            #client{sock = Socket, port = Port, host = Host, banner = BannerLine};
          _            -> {invalid_response_code, BannerLine}

        end;

      {error, Reason} -> {connection_error, Reason}
  end.

%% @doc Close connection to the dictd server.
-spec disconnect(client()) -> ok.
disconnect(#client{sock = Socket}) ->  gen_tcp:close(Socket).

%% @doc Return short info about the server.
-spec server_banner(client()) -> binary().
server_banner(#client{banner = Banner}) -> Banner.

%% @doc Retrieve a list of available databases on the server.
-spec databases(client()) -> [{binary(), binary()}].
databases(#client{sock = Socket}) ->
  Lines = request_response(Socket, <<"show db">>, 110, {554, response_no_databases}),
  lines_to_proplist(Lines).

%% @doc Retrieve a list of match strategies the server supports.
-spec strategies(client()) -> [{binary(), binary()}].
strategies(#client{sock = Socket}) ->
  Lines = request_response(Socket, <<"show strat">>, 111, {555, response_no_strategies}),
  lines_to_proplist(Lines).

%% @doc Retrieve server help text.
-spec server_help(client()) -> binary().
server_help(#client{sock = Socket}) ->
  unicode:characters_to_binary(request_response(Socket, <<"help">>, 113)).

%% @doc Retrieve server status.
-spec server_info(client()) -> binary().
server_info(#client{sock = Socket}) ->
  unicode:characters_to_binary(request_response(Socket, <<"show server">>, 114)).

%% @doc Retrieve word definitions from all dictionaries.
-spec define(client(), iolist()) -> [word_definition()].
define(Client, Word) -> define(Client, Word, ?ALL_DICTIONARIES).

%% @doc Retrieve word definitions from a given dictionary.
-spec define(client(), iolist(), iolist()) -> [word_definition()].
define(#client{sock = Socket}, Word, Database) ->

  Command = unicode:characters_to_binary(["define ", Database, " \"", Word, "\""]),

  % io:format("~ts~n", [Command]),

  case request_response(Socket, Command, 150, {552, no_match}) of
    no_match -> [];
    Lines ->
      lists:foldl(
        fun
          (Line, [])           ->
            {match, DefinedWord, Dict, DictDescription} = parse_as_word_definition_header(Line),
            [#word_definition{dictionary =  Dict, dictionary_description = DictDescription, word = DefinedWord}];

          (Line, Accu = [H|T]) ->
            case parse_as_word_definition_header(Line) of
              nomatch ->
                UpdatedWordDefinition =
                  H#word_definition{definition=unicode:characters_to_binary([H#word_definition.definition, Line])},
                [ UpdatedWordDefinition | T];
              {match, DefinedWord, Dict, DictDescription} ->
                NewWordDefinition = #word_definition{
                  dictionary =  Dict,
                  dictionary_description = DictDescription,
                  word = DefinedWord},
                [NewWordDefinition  | Accu]
            end
        end,
        [],
        Lines)
    end.

%% @doc Match a word using all strategies in all databases.
-spec match(client(), iolist()) -> [{binary(), binary()}].
match(Client, Word)             -> match(Client, Word, ?DEFAULT_STRATEGY).

%% @doc Match a word using a certain strategy in all databases.
-spec match(client(), iolist(), nonempty_string()) -> [{binary(), binary()}].
match(Client, Word, Strategy) -> match(Client, Word, Strategy, ?ALL_DICTIONARIES).

%% @doc Match a word using a certain strategy in a certain database.
-spec match(client(), iolist(), nonempty_string(), iolist()) -> [{binary(), binary()}].
match(#client{sock = Socket}, Word, Strategy, Database) ->

  Command = unicode:characters_to_binary(["match ", Database, " ", Strategy, " \"", Word, "\""]),

  case request_response(Socket, Command, 152, {552, no_match}) of
    no_match -> [];
    Lines -> lines_to_proplist(Lines)
  end.



%%%===================================================================
%%% Private
%%%===================================================================

%% @doc Read a line from the socket.
-spec readline(socket()) -> binary().
readline(Socket) ->
  {ok, Line} = gen_tcp:recv(Socket, 0),
  Line.

%% @doc Send a dict command.
-spec send_command(socket(), binary()) -> ok | {error, closed | inet:posix()}.
send_command(Socket, Line) ->
  % io:format("~ts~n", [Line]),
  gen_tcp:send(Socket, unicode:characters_to_binary([Line, ?EOL])).

%% @doc Extract a response code from a line.
-spec extract_reply_code(binary()) ->  pos_integer() | no_code.
extract_reply_code(Line) ->
  case re:run(Line, "^(\\d+)", [{capture, [1], list}] ) of
    {match, [Code]} -> list_to_integer(Code);
    _               -> no_code
  end.

%% @doc Send a command and read a response without defining any success of failure codes.
-spec request_response(socket(), binary(), integer()) -> atom() | [binary()].
request_response(Socket, Command, OkCode) ->
  request_response(Socket, Command, OkCode, {0, none}).

%% @doc Send a command and read a response.
-spec request_response(socket(), binary(), integer(), {integer(), atom()}) -> atom() | [binary()].
request_response(Socket, Command, OkCode, {NotOkCode, NotOkMeaning}) ->
  send_command(Socket, Command),

  ResponseHeaderLine = readline(Socket),

  case  extract_reply_code(ResponseHeaderLine) of
    OkCode           -> read_response_from_socket(Socket);
    NotOkCode        -> NotOkMeaning;
    SomethingElse    -> {unexpected_code, SomethingElse}
  end.

%% @doc Read a response from the socket.
-spec read_response_from_socket(socket()) ->  [binary()].
read_response_from_socket(Socket) ->
  lists:reverse(read_response_from_socket(Socket, [])).

%% @doc Read a response from the socket.
-spec read_response_from_socket(socket(), [binary()]) ->  [binary()].
read_response_from_socket(Socket, Lines) ->
  Line = readline(Socket),
  case  extract_reply_code(Line) of
    ?RESPONSE_OK -> Lines;
    _            ->
      if
        Line =:= ?EOD -> read_response_from_socket(Socket, Lines);
        true          -> read_response_from_socket(Socket, [Line|Lines])
      end
  end.

%% @doc Parse lines to a proplist.
-spec lines_to_proplist([binary()]) -> [{binary(), binary()}].
lines_to_proplist(Lines)->
  lists:filter(
    fun(Tuple) -> Tuple =/= nil end,
    lists:map(
      fun(Line) ->
        case re:run(Line, "^([^\s]+)\s\\\"(.+)\\\"", [{capture, [1, 2], binary}] ) of
          {match, [DictName, DictDescription]} -> {DictName, DictDescription};
          _               -> nil
        end
      end, Lines)).

%% @doc Try to parse a line as a word definition header.
-spec parse_as_word_definition_header(binary()) -> nomatch | { match, boolean(), boolean(), boolean()}.
parse_as_word_definition_header(Line) ->
  case re:run(Line, "^151 \\\"([^\\\"]+)\\\" ([^\\\"]+) \\\"([^\\\"]+)\\\"", [{capture, [1, 2, 3], binary}] ) of
    {match, [Word, Dict, DictDescription]} -> {match, Word, Dict, DictDescription};
    nomatch -> nomatch
  end.

% 151 "apple" sokrat_en-ru "sokrat_enru"

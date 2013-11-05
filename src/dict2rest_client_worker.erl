%% @author Yuri Leikind
%% @copyright 2013 Yuri Leikind.

%% @doc
%%   gen_server which wraps dict2rest_dict_client and provides API for all resources.
%%
%%   The public API caches strategies and dictionaries in ETS and reads
%%   them without sending a message to the gen_server process.
%%
%%   The client is created on demand and is disconnected after a timeout.
%%
%% @end

-module(dict2rest_client_worker).

-behaviour(gen_server).

% TODO: define for a certain library

-record(state, {
  client = undefined  :: dict2rest_dict_client:client() | undefined,
  host                :: nonempty_string(),
  port                :: pos_integer(),
  define_counter = 0  :: integer(),
  match_counter  = 0  :: integer()
}).



% External API
-export([
         start_link/2,
         dictionaries/0,
         strategies/0,
         define/2,
         match/2,
         match/3,
         match_counter/0,
         define_counter/0
]).


-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(DISCONNECT_TIMEOUT, 2000).

% External API
start_link(Host, Port) ->
  gen_server:start_link({ local, ?MODULE }, ?MODULE, {Host, Port}, []).


dictionaries() ->
  dictionaries_or_strategies(dictionaries).

strategies() ->
  dictionaries_or_strategies(strategies).

dictionaries_or_strategies(Type) ->
  case ets:lookup(dict2rest_cache, Type) of
    []                                     -> gen_server:call(?MODULE, Type);
    [{Type, Timestamp, RetrievedObjects}]  ->

      {DaysDiff, _TimeDiff} =  calendar:time_difference(Timestamp, calendar:local_time()),
      if
        DaysDiff > 0 -> gen_server:call(?MODULE, Type);
        true         -> RetrievedObjects
      end
  end.


define(Word, Dictionary) ->
    gen_server:call(?MODULE, {define, Word, Dictionary}).


match(Word, Strategy) ->
    gen_server:call(?MODULE, {match, Word, Strategy}).


match(Word, Strategy, Dictionary) ->
    gen_server:call(?MODULE, {match, Word, Strategy, Dictionary}).


match_counter() ->
    gen_server:call(?MODULE, match_counter).


define_counter() ->
    gen_server:call(?MODULE, define_counter).


% gen_server callbacks

init({Host, Port})  ->
  ets:new(dict2rest_cache, [set, named_table]),
  {ok, #state{port = Port, host = Host} }.

handle_call(dictionaries, _From,  State) ->
  NewClient = connect(State),
  Dictionaries = dict2rest_dict_client:databases(NewClient),
  ets:insert(dict2rest_cache, {dictionaries, calendar:local_time(), Dictionaries}),
  {reply, Dictionaries, State#state{client = NewClient}, ?DISCONNECT_TIMEOUT};

handle_call(strategies, _From, State) ->
  NewClient = connect(State),
  Strategies = dict2rest_dict_client:strategies(NewClient),
  ets:insert(dict2rest_cache, {strategies, calendar:local_time(), Strategies}),
  {reply, Strategies, State#state{client = NewClient}, ?DISCONNECT_TIMEOUT};


handle_call({define, Word, Dictionary}, _From, State) ->
  NewClient = connect(State),
  Definitions = dict2rest_dict_client:define(NewClient, Word, Dictionary),
  {reply, Definitions, State#state{client = NewClient, define_counter = State#state.define_counter + 1}, ?DISCONNECT_TIMEOUT};

% TODO: refactor
handle_call({match, Word, Strategy}, _From, State) ->
  NewClient = connect(State),
  Matches = dict2rest_dict_client:match(NewClient, Word, Strategy),
  {reply, Matches, State#state{client = NewClient, match_counter = State#state.match_counter + 1}, ?DISCONNECT_TIMEOUT};

handle_call({match, Word, Strategy, Dictionary}, _From, State) ->
  NewClient = connect(State),
  Matches = dict2rest_dict_client:match(NewClient, Word, Strategy, Dictionary),
  {reply, Matches, State#state{client = NewClient, match_counter = State#state.match_counter + 1}, ?DISCONNECT_TIMEOUT};



handle_call(match_counter, _From, #state{match_counter = Counter} = State) ->
  {reply, Counter, State, ?DISCONNECT_TIMEOUT};

handle_call(define_counter, _From, #state{define_counter = Counter} = State) ->
  {reply, Counter, State, ?DISCONNECT_TIMEOUT}.


handle_cast(_, State) ->  {noreply, State}.


terminate(_Reason, #state{client = undefined}) ->  ok;
terminate(_Reason, #state{client = Client}) ->
  dict2rest_dict_client:disconnect(Client),
  ok.

handle_info(timeout, #state{client = undefined} =  State) ->
    % io:format("not disconnecting~n"),
    {noreply, State};

handle_info(timeout, #state{client = Client} =  State) ->
    % io:format("disconnecting~n"),
    dict2rest_dict_client:disconnect(Client),
    {noreply, State#state{client = undefined}};

handle_info(_, State) ->  {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

% Internal stuff


connect(#state{client = undefined, host = Host, port = Port}) ->
  % io:format("connecting~n"),
  dict2rest_dict_client:connect(Host, Port);
connect(#state{client = Client})  -> Client.


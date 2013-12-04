-record(word_definition, {
  dictionary               :: binary(),
  dictionary_description   :: binary(),
  word                     :: binary(),
  definition = <<"">>      :: binary()
}).

-define(ALL_DICTIONARIES, "*").
-define(ALL_STRATEGIES, ".").

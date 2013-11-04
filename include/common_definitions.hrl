-record(word_definition, {
  dictionary               :: binary(),
  dictionary_description   :: binary(),
  word                     :: binary(),
  definition = <<"">>      :: binary()
}).

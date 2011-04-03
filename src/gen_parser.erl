-module(gen_parser).
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{parse, 1}];
behaviour_info(_) ->
    undefined.


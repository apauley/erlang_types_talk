-module(dynamic).

-export([example/1]).

example({}) ->
  "empty tuple";
example(List) when is_list(List) ->
  length(List);
example(_Else) ->
  another_return_type.

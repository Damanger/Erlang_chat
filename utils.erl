-module(utils).

-export([set_cookie/0, list_contains/2]).

set_cookie() -> erlang:set_cookie(node(), default).

-spec(list_contains(Element :: term(), List :: [term()]) -> boolean()).
list_contains(Element, List) -> lists:any(fun(E) -> (E =:= Element) end, List).

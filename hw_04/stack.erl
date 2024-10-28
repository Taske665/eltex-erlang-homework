-module(stack).
-export([new/0, add/2, pop/1, peek/1, size_s/1, is_empty/1]).
-include_lib("eunit/include/eunit.hrl").

-record(stack, {
    status,
    value,
    stack
}).

-type stack() :: #stack{status :: atom(), value :: _, stack :: empty | list()}.

-spec new() -> stack().
new() -> 
    #stack{status = ok, value = "", stack = empty}.

-spec add(stack(), _) -> stack().
add(S, Val) ->
    case S#stack.stack of
        empty -> 
            #stack{status = ok, value = "", stack = [Val]};
        _ -> 
            #stack{status = ok, value = "", stack = [Val | S#stack.stack]}
    end.

-spec pop(stack()) -> stack().
pop(S) ->
    case S#stack.stack of
        empty -> 
            #stack{status = error, value = "Stack is empry.", stack = empty};
        [Head | Tail] ->
            case Tail of
                [] -> 
                    #stack{status = ok, value = Head, stack = empty};
                _ -> 
                    #stack{status = ok, value = Head, stack = Tail}
            end
    end.

-spec peek(stack()) -> _.
peek(S) ->
    case S#stack.stack of
        empty -> 
            "Stack is empry.";
        [Head | _] -> 
            Head
    end.

-spec size_s(stack()) -> integer().
size_s(S) -> size_s(S#stack.stack, 0).

-spec size_s(list(), integer()) -> integer().
size_s(Stack,L) ->
    case Stack of
        empty ->
            0;
        [] -> 
            L;
        [_ | Tail] ->
            size_s(Tail, L+1)
    end.

-spec is_empty(stack()) -> boolean().
is_empty(S) ->
    case S#stack.stack of
        empty ->
            true;
        [] -> 
            true;
        _ -> 
            false
    end.

new_test() ->
    ?assertEqual({stack, ok, [], empty}, new()),
    ok.

add_test() ->
    ?assertEqual({stack, ok, [], [5]}, add(new(), 5)),
    ?assertEqual({stack, ok, [], [10, 5]}, add(add(new(), 5), 10)),
    ?assertEqual({stack, ok, [], [2, 10, 5]}, add(add(add(new(), 5), 10), 2)),
    ok.

size_s_test() ->
    ?assertEqual(3, size_s(add(add(add(new(), 5), 10), 2))),
    ?assertEqual(0, size_s(new())),
    ok.

peek_test() ->
    ?assertEqual(2, peek(add(add(add(new(), 5), 10), 2))),
    ?assertEqual("Stack is empry.", peek(new())),    
    ok.

pop_test() ->
    X = add(add(add(new(), 5), 10), 2),
    ?assertEqual({stack, ok, 2, [10, 5]}, pop(X)),
    ?assertEqual({stack, ok, 10, [5]}, pop(pop(X))),
    ?assertEqual({stack, ok, 5, empty}, pop(pop(pop(X)))),
    ?assertEqual({stack, error, "Stack is empry.", empty}, pop(pop(pop(pop(X))))),
    ok.

is_empty_test() ->
    ?assertEqual(true, is_empty(new())),
    ?assertEqual(true, is_empty(pop(add(new(), 5)))),
    ?assertEqual(false, is_empty(add(new(),8))),
    ok.
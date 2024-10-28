-module(q).
-export([new/0, add/2, pop/1, peek/1, size_q/1, is_empty/1]).
-include_lib("eunit/include/eunit.hrl").

-record(queue, {
    status,
    value,
    queue
}).

-type queue() :: #queue{status :: atom(), value :: _, queue :: empty | list()}.

-spec new() -> queue().
new() -> 
    #queue{status = ok, value = "", queue = empty}.

-spec add(queue(), _) -> queue().
add(Q, Val) ->
    case Q#queue.queue of
        empty -> 
            #queue{status = ok, value = "", queue = [Val]};
        _ -> 
            #queue{status = ok, value = "", queue = Q#queue.queue ++ [Val]}
    end.

-spec pop(queue()) -> queue().
pop(Q) ->
    case Q#queue.queue of
        empty -> 
            #queue{status = error, value = "Queue is empry.", queue = empty};
        [Head | Tail] ->
            case Tail of
                [] -> 
                    #queue{status = ok, value = Head, queue = empty};
                _ -> 
                    #queue{status = ok, value = Head, queue = Tail}
            end
    end.

-spec peek(queue()) -> _.
peek(Q) ->
    case Q#queue.queue of
        empty -> 
            "Queue is empry.";
        [Head | _] -> 
            Head
    end.

-spec size_q(queue()) -> integer().
size_q(Q) -> size_q(Q#queue.queue, 0).

-spec size_q(list(), integer()) -> integer().
size_q(Queue,L) ->
    case Queue of
        empty ->
            0;
        [] -> 
            L;
        [_ | Tail] ->
            size_q(Tail, L+1)
    end.

-spec is_empty(queue()) -> boolean().
is_empty(Q) ->
    case Q#queue.queue of
        empty ->
            true;
        [] -> 
            true;
        _ -> 
            false
    end.

new_test() ->
    ?assertEqual({queue, ok, [], empty}, new()),
    ok.

add_test() ->
    ?assertEqual({queue, ok, [], [5]}, add(new(), 5)),
    ?assertEqual({queue, ok, [], [5, 10]}, add(add(new(), 5), 10)),
    ?assertEqual({queue, ok, [], [5, 10, 2]}, add(add(add(new(), 5), 10), 2)),
    ok.

size_q_test() ->
    ?assertEqual(3, size_q(add(add(add(new(), 5), 10), 2))),
    ?assertEqual(0, size_q(new())),
    ok.

peek_test() ->
    ?assertEqual(5, peek(add(add(add(new(), 5), 10), 2))),
    ?assertEqual("Queue is empry.", peek(new())),    
    ok.

pop_test() ->
    X = add(add(add(new(), 5), 10), 2),
    ?assertEqual({queue, ok, 5, [10, 2]}, pop(X)),
    ?assertEqual({queue, ok, 10, [2]}, pop(pop(X))),
    ?assertEqual({queue, ok, 2, empty}, pop(pop(pop(X)))),
    ?assertEqual({queue, error, "Queue is empry.", empty}, pop(pop(pop(pop(X))))),
    ok.

is_empty_test() ->
    ?assertEqual(true, is_empty(new())),
    ?assertEqual(true, is_empty(pop(add(new(), 5)))),
    ?assertEqual(false, is_empty(add(new(),8))),
    ok.
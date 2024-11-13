-module(sort).
-export([sort/2]).
-include_lib("eunit/include/eunit.hrl").

-spec sort(list(), atom()) -> list().
sort(List, Atr) ->
    case List of
        [] -> [];
        [_ = Single_element] -> [Single_element];
        [Head | Tail] -> 
            List1 = [X || X <- Tail, maps:get(Atr, X) < maps:get(Atr, Head)],
            List2 = [X || X <- Tail, maps:get(Atr, X) >= maps:get(Atr, Head)],
            sort(List1, Atr) ++ [Head] ++ sort(List2, Atr)
    end.

sort_test() ->
    List = [
        #{first_name => "Alice", last_name => "Smith", age => 19, grade => 4.5}, 
        #{first_name => "Dylan", last_name => "Ivanov", age => 21, grade => 4.9},
        #{first_name => "Bob", last_name => "Brawn", age => 18, grade => 4.7},
        #{first_name => "Michel", last_name => "Adams", age => 26, grade => 4.3},
        #{first_name => "Jack", last_name => "Silverstone", age => 24, grade => 3.9},
        #{first_name => "Tom", last_name => "Kerry", age => 36, grade => 4.6},
        #{first_name => "Alex", last_name => "Luice", age => 22, grade => 4.8}],
    Sorted_first_name = [
        #{first_name => "Alex", last_name => "Luice", age => 22, grade => 4.8},
        #{first_name => "Alice", last_name => "Smith", age => 19, grade => 4.5},
        #{first_name => "Bob", last_name => "Brawn", age => 18, grade => 4.7},
        #{first_name => "Dylan", last_name => "Ivanov", age => 21, grade => 4.9},
        #{first_name => "Jack", last_name => "Silverstone", age => 24, grade => 3.9},
        #{first_name => "Michel", last_name => "Adams", age => 26, grade => 4.3},
        #{first_name => "Tom", last_name => "Kerry", age => 36, grade => 4.6}],
    Sorted_last_name = [
        #{first_name => "Michel", last_name => "Adams", age => 26, grade => 4.3},
        #{first_name => "Bob", last_name => "Brawn", age => 18, grade => 4.7},
        #{first_name => "Dylan", last_name => "Ivanov", age => 21, grade => 4.9},
        #{first_name => "Tom", last_name => "Kerry", age => 36, grade => 4.6},
        #{first_name => "Alex", last_name => "Luice", age => 22, grade => 4.8},
        #{first_name => "Jack", last_name => "Silverstone", age => 24, grade => 3.9},
        #{first_name => "Alice", last_name => "Smith", age => 19, grade => 4.5}],
    Sorted_age = [
        #{first_name => "Bob", last_name => "Brawn", age => 18, grade => 4.7},
        #{first_name => "Alice", last_name => "Smith", age => 19, grade => 4.5},
        #{first_name => "Dylan", last_name => "Ivanov", age => 21, grade => 4.9},
        #{first_name => "Alex", last_name => "Luice", age => 22, grade => 4.8},
        #{first_name => "Jack", last_name => "Silverstone", age => 24, grade => 3.9},
        #{first_name => "Michel", last_name => "Adams", age => 26, grade => 4.3},
        #{first_name => "Tom", last_name => "Kerry", age => 36, grade => 4.6}],
    Sorted_grade = [
        #{first_name => "Jack", last_name => "Silverstone", age => 24, grade => 3.9},
        #{first_name => "Michel", last_name => "Adams", age => 26, grade => 4.3},
        #{first_name => "Alice", last_name => "Smith", age => 19, grade => 4.5},
        #{first_name => "Tom", last_name => "Kerry", age => 36, grade => 4.6},
        #{first_name => "Bob", last_name => "Brawn", age => 18, grade => 4.7},
        #{first_name => "Alex", last_name => "Luice", age => 22, grade => 4.8},
        #{first_name => "Dylan", last_name => "Ivanov", age => 21, grade => 4.9}],
    ?assertEqual(Sorted_first_name, sort(List, first_name)),
    ?assertEqual(Sorted_last_name, sort(List, last_name)),
    ?assertEqual(Sorted_age, sort(List, age)),
    ?assertEqual(Sorted_grade, sort(List,grade)),
    ok.
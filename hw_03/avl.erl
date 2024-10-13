-module(avl).
-export([new/0, add/2, search/2, del/2, in_order/1, pre_order/1, post_order/1, add_list/1]).

-record(node, {
    val, 
    left = empty, 
    right = empty,
    height = 1
}).

%% Для определения пользовательского типа данных, как было в примере на лекции, 
%% я не понял практического применения создаваемого типа в контексте данной задачи.
%% Поэтому не стал его определять.

%% Создание нового пустого дерева.
new() ->
    empty.

%% Для упрощения тестирования добавил функцию, создающую дерево на основе списка значений.
add_list(List) ->
    case is_list(List) of
        true ->
            lists:foldl(fun(Val, Tree) -> add(Tree, Val) end, empty, List);
        false ->
            io:fwrite("List data type expected ~n")
    end.

%% Вставка значений.
add(Tree, NewVal) ->
    case is_integer(NewVal) orelse is_float(NewVal) of
        true ->
        case Tree of
            empty -> 
                #node{val = NewVal, height = 1};
            _ ->
            case NewVal < Tree#node.val of
                true ->
                    NewLeft = add(Tree#node.left, NewVal),
                    NewTree = new_height(Tree#node{left = NewLeft}),
                    balance(NewTree);
                false ->
                    case NewVal > Tree#node.val of
                        true ->
                            NewRight = add(Tree#node.right, NewVal),
                            NewTree = new_height(Tree#node{right = NewRight}),
                            balance(NewTree);
                        false ->
                            Tree
                    end
            end
        end;
        false ->
            io:fwrite("Integer or float data type expected ~n")
    end.

%% Возвращает значение высоты текущего узла.
height(Tree) ->
    case Tree of
        empty ->
            0;
        #node{height = Height} ->
            Height
    end.

%% Определение и перезапись высоты поддеревьев для узла.
new_height(Tree) ->
    NewHeight = 1 + max(height(Tree#node.left), height(Tree#node.right)),
    Tree#node{height = NewHeight}.

%% Перестановка узлов вправо.
rr(Tree) ->
    LB = Tree#node.left, %левая ветвь исходного поддерева
    RB = Tree#node.right, %правая ветвь исходного поддерева
    LLB = LB#node.left, %левая ветвь левой ветви
    RLB = LB#node.right, %правая ветвь левой ветви
    NRB = new_height(#node{val = Tree#node.val, left = RLB, right = RB}), %новая правая ветвь с пересчитанной высотой
    new_height(#node{val = LB#node.val, left = LLB, right = NRB}).    

%% Перестановка узлов влево (аналогично перестановке вправо, только зеркально).
rl(Tree) ->
    LB = Tree#node.left, %левая ветвь исходного поддерева
    RB = Tree#node.right, %правая ветвь исходного поддерева
    LRB = RB#node.left, %левая ветвь правой ветви
    RRB = RB#node.right, %правая ветвь правой ветви
    NLB = new_height(#node{val = Tree#node.val, left = LB, right = LRB}), %новая левая ветвь с пересчитанной высотой
    new_height(#node{val = RB#node.val, left = NLB, right = RRB}).

%% Критерий проверки на сбалансированность, "+" левое поддерево длинее, "-" - правое.
height_dif(Tree) ->
    height(Tree#node.left) - height(Tree#node.right).

%% Проверка и балансировка.
balance(Tree) ->
    case height_dif(Tree) of
        2 -> 
            case height_dif(Tree#node.left) < 0 of %здесь проверяется, что функция перестановки не перезапишет полезные данные, за счет затирания правого подподдерева левого поддерева
                true ->
                    NewTree = Tree#node{left = rl(Tree#node.left)}, %если перезапишет, то сначала перестановкой освобождается нужна для работы функции подподветвь
                    rr(NewTree);
                false ->
                    rr(Tree)
            end;
        -2 ->
            case height_dif(Tree#node.right) > 0 of
                true -> %аналогично функционалу чуть выше
                    NewTree = Tree#node{right = rr(Tree#node.right)},
                    rl(NewTree);    
                false ->
                    rl(Tree)
            end;
        _ -> Tree
    end.

%% Удаление значения из дерева
del(Tree, _) when Tree == empty ->
        Tree;
del(Tree, Val) ->
    if Val == Tree#node.val ->
        {Left, Right} = {Tree#node.left, Tree#node.right},
        case {Left, Right} of
            {empty, empty} ->
                empty;
            {_, empty} -> %если у узла только одна ветвь, то подставляет ее на место удаляемого узла
                NewTree = #node{val = Left#node.val, left = Left#node.left, right = Left#node.right},
                new_height(NewTree),
                balance(NewTree);
            {empty, _} ->
                NewTree = #node{val = Right#node.val, left = Right#node.left, right = Right#node.right},
                new_height(NewTree),
                balance(NewTree);
            {_, _} -> %если у узла обе ветви, то в правой ветви находит минимальное значение, подстановка которого на место удаляемого узла не нарушит структуры дерева (аналогично можно было бы подставить максимальное значение из левой ветви)
                NewVal = minright(Right),
                NewRight = del(Right, NewVal),
                NewTree = #node{val = NewVal, left = Left, right = NewRight},
                new_height(NewTree),
                balance(NewTree)
        end;
    Val < Tree#node.val ->
        NewTree = Tree#node{left = del(Tree#node.left, Val)},
        new_height(NewTree),
        balance(NewTree);
    Val > Tree#node.val ->
        NewTree = Tree#node{right = del(Tree#node.right, Val)},
        new_height(NewTree),
        balance(NewTree)
    end.

%% Вспомогательная функция для поиска нового значения взамен удаляемого
minright(Tree) ->
    case Tree#node.left of
        empty ->
            Tree#node.val;
        _ ->
            minright(Tree#node.left)
    end.

%% Поиск значения в дереве.
search(Tree, Val) ->
    if Val == Tree#node.val ->
        {found, Tree#node.val};
    Val < Tree#node.val ->
        search(Tree#node.left, Val);
    Val > Tree#node.val ->
        search(Tree#node.right, Val);
    true ->
        {not_found}
    end.

%% Обходы дерева в разных порядках и вывод значений списком.
%% Возвращает значения в порядке возрастания.
in_order(Tree) ->
    case Tree of
        empty -> [];
        _ ->
            Left = in_order(Tree#node.left),
            Right = in_order(Tree#node.right),
            Left ++ [Tree#node.val] ++ Right
    end.

%% Проход от корня списка до крайних листов от левых ветвей к правым.
pre_order(Tree) ->
    case Tree of
        empty -> [];
        _ ->
            Left = pre_order(Tree#node.left),
            Right = pre_order(Tree#node.right),
            [Tree#node.val] ++ Left ++ Right
    end.

%% Проход от самого левого листа слева направо к корню дерева.
post_order(Tree) ->
    case Tree of
        empty -> [];
        _ ->
            Left = post_order(Tree#node.left),
            Right = post_order(Tree#node.right),
            Left ++ Right ++ [Tree#node.val]
    end.
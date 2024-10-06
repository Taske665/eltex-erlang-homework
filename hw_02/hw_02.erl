% Задание №1.
Persons = [{person, 1, "Bob", 23, male}, {person, 2, "Kate", 20, female}, {person, 3, "Jack", 34, male}, {person, 4, "Nata", 54, female}].
[First | Rest] = Persons.
First.
Rest.
%Выполнено разделение значений, содержащихся в списке Persons. В переменную First записалось значение первого элемента списка Persons, в переменную Rest записался список из остальных значений списка Persons.
[Second | Rest1] = Rest.
26> Second.
27> Rest1.
%Операция аналогичная первому разделению списка, только на этот раз для списка Rest.
[Third, Fourth | Rest2] = Rest1.
Third.
Fourth.
Rest2.
%В переменные Third и Fourth записались значения первого и второго элементов списка Rest1, в переменную Rest2 - пустой список, указывающий в списке Rest1 на его конец.
Persons.
%Исходный список действительно не поменялся:)

% Задание №2.
rr("person.hrl").
f(Persons).
Persons = [#person{id = 1, name = "Bob", age = 23, gender = male}, #person{id = 2, name = "Kate", age = 20, gender = female}, #person{id = 3, name = "Jack", age = 34, gender = male}, #person{id = 4, name = "Nata", age = 54, gender = female}].
[FirstPerson | _] = Persons.
FirstPerson.
%Переменной FirstPerson присвоено значение первого элемента списка Persons.
[_, SecondPerson, _, _] = Persons.
SecondPerson.
%Переменной SecondPerson присвоено значение второго элемента списка Persons.
[_, _, SecondPerson | _] = Persons.
%Т.к. переменной SecondPerson уже присвоено значение, то происходит сопоставление списков. При этом сравниваются только значения третьих элементов списков. Выдается сообщение, что они не совпадают.
SecondName = SecondPerson#person.name.
SecondAge = SecondPerson#person.age.
%Переменным присваеваются значения на основе данных переменной SecondPerson, соответствующие позиции с указанным ключом/именем.
Persons.
SecondPerson#person{age = 21}.
Persons.
SecondPerson.

% Задание №3 повторяет задание №3 из первой домашней работы, вероятно, это ошибка.
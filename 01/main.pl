:- include('../utilities/read_input.pl').
:- initialization(main).

elf(elf(Snacks)) --> snacks(Snacks), [''].
elf(elf(Snacks)) --> snacks(Snacks).
snack(N) --> [N_string], { atom_codes(N_string, AsciiRep), member(_, AsciiRep), number_codes(N, AsciiRep) }.
snacks([N|Ns]) --> snack(N), snacks(Ns),!.
snacks([N]) -->  snack(N),!.
elves([]) --> [].
elves([E|Es]) --> elf(E), elves(Es).

sum_calories(elf([]), 0).
sum_calories(elf([N|T]), SumCalories) :-
	sum_calories(elf(T), M),
	SumCalories is N + M.

main :-
	readFile('input.txt', Input),

	once(elves(Elves, Input, [])),
	maplist(sum_calories, Elves, CarriedCalories),
	msort(CarriedCalories, CarriedCaloriesAsc),
	reverse(CarriedCaloriesAsc, CarriedCaloriesDesc),

	%% part 1
	nth0(0, CarriedCaloriesDesc, MaxCalories),
	write(MaxCalories), nl,

	%% part2
	append([X, Y, Z], _, CarriedCaloriesDesc),
	sum_list([X, Y, Z], SumTopThreeCalories),
	write(SumTopThreeCalories), nl,
	halt.

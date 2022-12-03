:- include('../utilities/read_input.pl').
:- initialization(main).

compartmentContents(Rucksack, Compartment1, Compartment2) :-
	length(Rucksack, TotalNumberOfItems),
	ItemsPerCompartment is TotalNumberOfItems div 2,
	length(Compartment1, ItemsPerCompartment),
	length(Compartment2, ItemsPerCompartment),
	append(Compartment1, Compartment2, Rucksack).

duplicatedItem(Compartment1, Compartment2, Item) :-
	member(Item, Compartment1),
	member(Item, Compartment2).

priorityOfItem(Item, Priority) :-
		(
  		Item > 96,
			Priority is Item - 96,
			!
		)
	;
		Priority is Item - 38.

priorityOfDuplicatedItem(Rucksack, Priority) :-
	compartmentContents(Rucksack, Compartment1, Compartment2),
	duplicatedItem(Compartment1, Compartment2, Item),
	priorityOfItem(Item, Priority).

groupBadge(Rucksack1, Rucksack2, Rucksack3, Badge) :-
	duplicatedItem(Rucksack1, Rucksack2, Badge),
	duplicatedItem(Rucksack1, Rucksack3, Badge),
	duplicatedItem(Rucksack3, Rucksack3, Badge).

badges([], []).
badges(Input, [B|Bs]) :-
	append([Rucksack1, Rucksack2, Rucksack3], RemainingInput, Input),
	groupBadge(Rucksack1, Rucksack2, Rucksack3, B),
	badges(RemainingInput, Bs).

main :-
	readFile('input.txt', Input),
	maplist(atom_codes, Input, InputCharCodes),

	%% part 1
	maplist(priorityOfDuplicatedItem, InputCharCodes, Priorities),
	sum_list(Priorities, SumOfPriorities),
	write(SumOfPriorities), nl,

	%% part 2
	badges(InputCharCodes, Badges),
	maplist(priorityOfItem, Badges, BadgePriorities),
	sum_list(BadgePriorities, SumOfBadgePriorities),
	write(SumOfBadgePriorities), nl,

	halt.

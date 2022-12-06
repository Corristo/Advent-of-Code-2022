:- include('../utilities/read_input.pl').
:- initialization(main).

pairwiseDisjoint([_]).
pairwiseDisjoint([H|T]) :-
	\+ member(H, T),
	pairwiseDisjoint(T), !.

startOfPacketMarker(A, B, C, D) :-
	pairwiseDisjoint([A, B, C, D]).

startOfMessageMarker(L) :-
	length(L, 14),
	pairwiseDisjoint(L).

startOfPacketMarker(Data, Index) :-
	startOfPacketMarker([], Data, Index).

startOfPacketMarker(DataRead, [A, B, C, D|_], Index) :-
	startOfPacketMarker(A, B, C, D),
	length(DataRead, L),
	Index is L + 4.
startOfPacketMarker(DataRead, [A, B, C, D|T], Index) :-
	\+ startOfPacketMarker(A, B, C, D),
	append(DataRead, [A], NewDataRead),
	startOfPacketMarker(NewDataRead, [B, C, D|T], Index).

startOfMessageMarker(Data, Index) :-
	startOfMessageMarker([], Data, Index).

startOfMessageMarker(DataRead, [A, B, C, D, E, F, G, H, I, J, K, L, M, N|T], Index) :-
	\+ startOfMessageMarker([A, B, C, D, E, F, G, H, I, J, K, L, M, N]),
	append(DataRead, [A], NewDataRead),
	startOfMessageMarker(NewDataRead, [B, C, D, E, F, G, H, I, J, K, L, M, N | T], Index).
startOfMessageMarker(DataRead, [A, B, C, D, E, F, G, H, I, J, K, L, M, N|_], Index) :-
	startOfMessageMarker([A, B, C, D, E, F, G, H, I, J, K, L, M, N]),
	length(DataRead, Length),
	Index is Length + 14.

main :-
	readFileChars('input.txt', Input),

	%% part 1
	startOfPacketMarker(Input, I),
	write(I), nl,

	%% part 2
	startOfMessageMarker(Input, I2),
	write(I2), nl,
	halt.

:- include('../utilities/read_input.pl').
:- initialization(main).

digit(1) --> ['1'].
digit(2) --> ['2'].
digit(3) --> ['3'].
digit(4) --> ['4'].
digit(5) --> ['5'].
digit(6) --> ['6'].
digit(7) --> ['7'].
digit(8) --> ['8'].
digit(9) --> ['9'].
digit(0) --> ['0'].
num(N) --> digit(D), num(D, N).
num(N, N) --> [].
num(A, N) --> digit(D), { A1 is A * 10 + D}, num(A1, N).

crate(crate(X)) --> ['['], [X], [']'].
nocrate --> [' ', ' ', ' '].

row([nocrate|Cs]) --> nocrate, [' '], row(Cs).
row([C|Cs]) --> crate(C), [' '], row(Cs).
row([C]) --> crate(C), !.
row([nocrate]) --> nocrate, !.

rows([R|Rs]) --> row(R), ['\n'], rows(Rs).
rows([]) --> [].

move(move(Count, Source, Destination)) -->
	['m', 'o', 'v', 'e', ' '],
	num(Count),
	[' ', 'f', 'r', 'o', 'm', ' '],
	num(Source),
	[' ', 't', 'o', ' '],
	num(Destination),
	['\n'], !.

moves([M|Ms]) --> move(M), moves(Ms).
moves([]) --> [].

char(C) --> [C], { \+ char_code(C, 10) }.
line([C|Cs]) --> char(C), !, line(Cs).
line([]) --> [].

puzzleInput(puzzle(state(StartingPosition), moves(Moves))) -->
	rows(StartingPosition),
	line(_),
	['\n', '\n'],
	moves(Moves).

emptyMatrix([],[]).
emptyMatrix([_|T1],[[]|T2]):-emptyMatrix(T1,T2).

appendTransposed([],[],[]).
appendTransposed([X|T1],[],[[X]|T3]):-appendTransposed(T1,[],T3).
appendTransposed([X|T1],[R|T2],[C|T3]):-append(R,[X], C),appendTransposed(T1,T2,T3).

transposeMatrix([L|M],T):-emptyMatrix(L,A),transpose([L|M],T,A).
transpose([],T,T).
transpose([L|M],T,A):-appendTransposed(L,A,B),transpose(M,T,B).

removeLeadingNocrates([], []).
removeLeadingNocrates([nocrate|T], L) :- removeLeadingNocrates(T, L), !.
removeLeadingNocrates([X|T], [X|T]) :- \+ X = nocrate.

validRowIndex(state(S), I) :- length(S, N), MaxIndex is N - 1, between(0, MaxIndex, I).

all(_, []) :- !.
all(P, [H|T]) :- call(P, H), all(P, T).

rowIdentical([], [], _).
rowIdentical(S, T, Index) :- nth0(Index, S, Row), nth0(Index, T, Row).

indices(0, []) :- !.
indices(N, [I|Is]):- I is N - 1, indices(I, Is).

removeFirst(_, [], []).
removeFirst(E, [E|T], T).
removeFirst(E, [H|T], [H|T2]) :- E \= H, removeFirst(E, T, T2).

applyMove(part(1), move(1,From,To), state(S), state(T)) :-
	FromIndex is From - 1,
	ToIndex is To - 1,
	nth0(FromIndex, S, FromRow),
	nth0(ToIndex, S, ToRow),
	FromRow = [crate(X)|NewFromRow],
	NewToRow = [crate(X)|ToRow],
	length(S, NumRows),
	length(T, NumRows),
	indices(NumRows, AllRowIndices),
	removeFirst(FromIndex, AllRowIndices, RowIndices1),
	removeFirst(ToIndex, RowIndices1, RowIndices),
	nth0(FromIndex, T, NewFromRow),
	nth0(ToIndex, T, NewToRow),
	all(rowIdentical(S, T), RowIndices).

applyMove(part(1), move(N,From,To), state(S), state(T)) :-
	applyMove(part(1), move(1,From,To), state(S), state(T1)),
	N1 is N - 1,
	applyMove(part(1), move(N1,From,To), state(T1), state(T)).

applyMove(part(2), move(N,From,To), state(S), state(T)) :-
	FromIndex is From - 1,
	ToIndex is To - 1,
	nth0(FromIndex, S, FromRow),
	nth0(ToIndex, S, ToRow),
	length(ItemsToMove, N),
	append(ItemsToMove, NewFromRow, FromRow),
	append(ItemsToMove, ToRow, NewToRow),
	length(S, NumRows),
	length(T, NumRows),
	indices(NumRows, AllRowIndices),
	removeFirst(FromIndex, AllRowIndices, RowIndices1),
	removeFirst(ToIndex, RowIndices1, RowIndices),
	nth0(FromIndex, T, NewFromRow),
	nth0(ToIndex, T, NewToRow),
	all(rowIdentical(S, T), RowIndices).

applyMoves(_, puzzle(state(S), moves([])), state(S)).
applyMoves(Part, puzzle(state(S), moves([M|Ms])), state(T)) :-
	applyMove(Part, M, state(S), state(T1)), !,
	applyMoves(Part, puzzle(state(T1), moves(Ms)), state(T)).

printResult([]).
printResult([crate(X)|Cs]) :- write(X), printResult(Cs).

main :-
	readFileChars('input.txt', Input),
	puzzleInput(puzzle(state(S), moves(M)), Input, []),
	transposeMatrix(S, T),
	maplist(removeLeadingNocrates, T, T1),

	% part 1
	applyMoves(part(1), puzzle(state(T1), moves(M)), state(T2)),
	maplist(nth0(0), T2, FirstElements),
	printResult(FirstElements), nl,

	% part 2
	applyMoves(part(2), puzzle(state(T1), moves(M)), state(T3)),
	maplist(nth0(0), T3, FirstElements2),
	printResult(FirstElements2), nl.

:- include('../utilities/read_input.pl').
:- initialization(main).

%% grammar for part 1
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
range(N-M) --> num(N), ['-'], num(M).
elfPair(elfPair(R, S)) --> range(R), [','], range(S), ['\n'].
elfPairs([P|Ps]) --> elfPair(P), elfPairs(Ps).
elfPairs([]) --> [].

containedRange(elfPair(A-B,N-M)) :-
		A >= N, B =< M, !.
containedRange(elfPair(A-B,N-M)) :-
		N >= A, M =< B, !.

overlappingRange(Pair) :- containedRange(Pair), !.
overlappingRange(elfPair(A-B,N-_)) :-
	A =< N, B >= N, !.
overlappingRange(elfPair(A-B,_-M)) :-
	A =< M, B >= M, !.


main :-
	readFileChars('input.txt', Input),
	elfPairs(Pairs, Input, []),

	%% part 1
	once(findall(X, ( member(X, Pairs), containedRange(X) ), ContainedRanges)),
	length(ContainedRanges, NumberOfContainedRanges),
	write(NumberOfContainedRanges), nl,

	% part 2
	once(findall(X, ( member(X, Pairs), overlappingRange(X) ), OverlappingRanges)),
	length(OverlappingRanges, NumberOfOverlappingRanges),
	write(NumberOfOverlappingRanges), nl,
	halt.

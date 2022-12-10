:- include('../utilities/read_input.pl').

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
num(X) --> ['-'], digit(D), num(D, N), {X is -N}.
num(N) --> digit(D), num(D, N).
num(N, N) --> [].
num(A, N) --> digit(D), { A1 is A * 10 + D}, num(A1, N), !.

operation(noop) --> ['n','o', 'o', 'p','\n'].
operation(add(X)) --> ['a','d','d','x',' '], num(X), ['\n'].

operations([O|Os]) --> operation(O), !, operations(Os), !.
operations([]) --> [].

applyOperation(noop, [CurrentState | PreviousStates], [CurrentState, CurrentState | PreviousStates]).
applyOperation(add(X), [CurrentState | PreviousStates], [NewState, CurrentState, CurrentState | PreviousStates]) :-
	NewState is CurrentState + X.

signalStrength(CpuStates, I, Strength) :-
	I1 is I - 1,
	nth0(I1, CpuStates, RegisterValue),
	Strength is I * RegisterValue.

terminalChar(Column, SpriteCenter, Char) :-
	SpriteLeft is SpriteCenter - 1,
	SpriteRight is SpriteCenter + 1,
	(member(Column, [SpriteLeft, SpriteCenter, SpriteRight]) *-> Char = '#' ; Char = ' ').

chunk(ChunkSize, In, Out) :-
	length(FirstChunk, ChunkSize),
	length(In, InputSize),
	(
		(
			InputSize >= ChunkSize,
			append(FirstChunk, Remainder, In), !,
			chunk(ChunkSize, Remainder, RemainingOut),
			Out = [FirstChunk | RemainingOut]
		)
	;
		(
			InputSize < ChunkSize,
			Out = []
		)
	).

writeLineMap(In) :- string_chars(S, In), writeln(S).

main :-
	readFileChars('input.txt', Input),
	operations(Ops, Input, []),
	foldl(applyOperation, Ops, [1], CpuStatesReversed),
	reverse(CpuStatesReversed, CpuStates),

	%% part 1
	maplist(signalStrength(CpuStates), [20, 60, 100, 140, 180, 220], SampledSignalStrengths),
	sumlist(SampledSignalStrengths, SumOfSignalStrengths),
	write(SumOfSignalStrengths), nl,

	%% part 2
	setof(Pos, between(0, 39, Pos), CrtColumns),
	chunk(40, CpuStates, ChunkedCpuStates),
	maplist(maplist(terminalChar, CrtColumns), ChunkedCpuStates, TerminalRows),
	maplist(writeLineMap, TerminalRows).



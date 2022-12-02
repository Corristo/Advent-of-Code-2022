:- include('../utilities/read_input.pl').
:- initialization(main).

%% grammar for part 1
move(shape(rock)) --> ['A'] ; ['X'].
move(shape(paper)) --> ['B'] ; ['Y'].
move(shape(scissors)) --> ['C'] ; ['Z'].
roundWithPredeterminedShape(round(OpponentMove, YourMove)) --> move(OpponentMove), [' '], move(YourMove).
roundsWithPredeterminedShape([]) --> [].
roundsWithPredeterminedShape([R|Rs]) --> roundWithPredeterminedShape(R), roundsWithPredeterminedShape(Rs).

%% grammar extensions for part 2
outcome(outcome(lose)) --> ['X'].
outcome(outcome(draw)) --> ['Y'].
outcome(outcome(win)) --> ['Z'].
roundWithPredeterminedOutcome(round(OpponentMove, Outcome)) --> move(OpponentMove), [' '], outcome(Outcome).
roundsWithPredeterminedOutcome([]) --> [].
roundsWithPredeterminedOutcome([R|Rs]) --> roundWithPredeterminedOutcome(R), roundsWithPredeterminedOutcome(Rs).

winningShape(shape(rock), shape(paper)).
winningShape(shape(paper), shape(scissors)).
winningShape(shape(scissors), shape(rock)).

win(round(_, outcome(win))).
win(round(OpponentShape, YourShape)) :- winningShape(OpponentShape, YourShape).

draw(round(_, outcome(draw))).
draw(Round) :- \+ win(Round), \+lose(Round).

lose(round(_, outcome(lose))).
lose(round(OpponentShape, YourShape)) :- winningShape(YourShape, OpponentShape).

pointsForResult(Round, 0) :- lose(Round).
pointsForResult(Round, 3) :- draw(Round).
pointsForResult(Round, 6) :- win(Round).

pointsForShape(round(_, shape(rock)), 1).
pointsForShape(round(_, shape(paper)), 2).
pointsForShape(round(_, shape(scissors)), 3).
pointsForShape(round(OpponentShape, outcome(Outcome)), Points) :-
	pointsForShape(round(OpponentShape, shape(YourShape)), Points),
	pointsForResult(round(OpponentShape, outcome(Outcome)), PointsForResult),
	pointsForResult(round(OpponentShape, shape(YourShape)), PointsForResult).

pointsForRound(Round, Points) :-
	pointsForResult(Round, N),
	pointsForShape(Round, M),
	Points is N + M.

codeToChar(Code, Char) :- char_code(Char, Code).

main :-
	readFile('input.txt', Input),
	maplist(atom_codes, Input, InputCharCodes),
	flatten(InputCharCodes, InputCharCodesFlattened),
	maplist(codeToChar, InputCharCodesFlattened, InputChars),

	%% part 1
	once(roundsWithPredeterminedShape(RoundsPart1, InputChars, [])),
	maplist(pointsForRound, RoundsPart1, PointsPerRoundPart1),
	sum_list(PointsPerRoundPart1, TotalPointsPart1),
	write(TotalPointsPart1), nl,

	%% part 2
	once(roundsWithPredeterminedOutcome(RoundsPart2, InputChars, [])),
	maplist(pointsForRound, RoundsPart2, PointsPerRoundPart2),
	sum_list(PointsPerRoundPart2, TotalPointsPart2),
	write(TotalPointsPart2), nl,
	halt.

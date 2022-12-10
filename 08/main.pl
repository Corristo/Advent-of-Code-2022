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

treeRow([T|Ts]) --> digit(T), treeRow(Ts).
treeRow([]) --> ['\n'].

forrest([R|Rs]) --> treeRow(R), !, forrest(Rs).
forrest([]) --> [].

size(forrest([R|Rs]), W-H) :-
	length([R|Rs], H),
	length(R, W).

heightAt(I, J, forrest(F), Height) :-
	nth0(I, F, Row),
	nth0(J, Row, Height).

treeVisible(0, _, forrest(_)) :- !.
treeVisible(_, 0, forrest(_)) :- !.
treeVisible(I, _, forrest(F)) :-
	size(forrest(F), _-H),
	I is H - 1, !.
treeVisible(_, J, forrest(F)) :-
	size(forrest(F), W-_),
	J is W - 1, !.
treeVisible(I, J, forrest(F)) :-
	heightAt(I, J, forrest(F), OwnHeight),
	size(forrest(F), W-H),
	I1 is I - 1,
	I2 is I + 1,
	J1 is J - 1,
	J2 is J + 1,
	H1 is H - 1,
	W1 is W - 1,
	(
		(findall(X, (between(0, I1, X), heightAt(X, J, forrest(F), OtherHeight), OtherHeight >= OwnHeight), []), !)
	;
		(findall(X, (between(I2, H1, X), heightAt(X, J, forrest(F), OtherHeight), OtherHeight >= OwnHeight), []), !)
	;
		(findall(X, (between(0, J1, X), heightAt(I, X, forrest(F), OtherHeight), OtherHeight >= OwnHeight), []), !)
	;
		(findall(X, (between(J2, W1, X), heightAt(I, X, forrest(F), OtherHeight), OtherHeight >= OwnHeight), []), !)
	).

betweenReverse(N, M, K) :- N > M, K = N.
betweenReverse(N, M, K) :- N == M, !, K = N.
betweenReverse(N, M, K) :- N > M, N1 is N-1, betweenReverse(N1, M, K).

scenicScore(I, J, forrest(F), Score) :-
	heightAt(I, J, forrest(F), OwnHeight),
	size(forrest(F), W-H),
	I1 is I - 1,
	I2 is I + 1,
	J1 is J - 1,
	J2 is J + 1,
	H1 is H - 1,
	W1 is W - 1,
	(
		(
			once((between(I2, H1, I3), heightAt(I3, J, forrest(F), OtherHeightD), OtherHeightD >= OwnHeight)),
			D is I3 - I,
			!
		)
		; D is H1 - I
	),
	(
		(
			once((betweenReverse(I1, 0, I4), heightAt(I4, J, forrest(F), OtherHeightU), OtherHeightU >= OwnHeight)),
			U is I - I4,
			!
		)
		; U is I
	),
	(
		(
			once((between(J2, W1, J3), heightAt(I, J3, forrest(F), OtherHeightR), OtherHeightR >= OwnHeight)),
			R is J3 - J,
			!
		)
		; R is W1 - J
	),
	(
		(
			once((betweenReverse(J1, 0, J4), heightAt(I, J4, forrest(F), OtherHeightL), OtherHeightL >= OwnHeight)),
			L is J - J4,
			!
		)
		;
			L is J
	),
	Score is U * D * L * R.

main :-
	readFileChars('input.txt', Input),
	forrest(F, Input, []),
	size(forrest(F), W1-H1),
	W is W1 - 1,
	H is H1 - 1,

	%% part 1
	setof(I-J, (between(0, H, I), between(0, W, J), treeVisible(I, J, forrest(F))), VisibleTrees),
	length(VisibleTrees, NumberOfVisibleTrees),
	write(NumberOfVisibleTrees), nl,

	%% part 2
	setof(ScenicScore, I^J^(between(0, H, I), between(0, W, J), scenicScore(I, J, forrest(F), ScenicScore)), Scores),
	reverse(Scores, ScoresDesc),
	ScoresDesc = [MaxScore|_],
	write(MaxScore), nl.


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
num(N) --> digit(D), num(D, N).
num(N, N) --> [].
num(A, N) --> digit(D), { A1 is A * 10 + D}, num(A1, N).

direction(right) --> ['R'].
direction(left) --> ['L'].
direction(up) --> ['U'].
direction(down) --> ['D'].

motion(motion(Direction, NumberOfSteps)) --> direction(Direction), [' '], num(NumberOfSteps), ['\n'].

motions([M | Ms]) --> motion(M), !, motions(Ms).
motions([]) --> [].

decompressMotions([], []).
decompressMotions([motion(X, 1) | Motions], [motion(X, 1)| DecompressedMotions]) :-
  decompressMotions(Motions, DecompressedMotions), !.
decompressMotions([motion(X, N) | Motions], [motion(X, 1)| DecompressedMotions]) :-
  N1 is N - 1,
  decompressMotions([motion(X, N1) | Motions], DecompressedMotions).

adjacent(HX-HY, TX-TY) :-
  abs(HX-TX) =< 1,
  abs(HY-TY) =< 1.

newTailPos(HX-Y, TX-Y, NTX-Y) :-
	NTX is HX + sign(TX-HX), !.
newTailPos(X-HY, X-TY, X-NTY) :-
	NTY is HY + sign(TY-HY), !.
newTailPos(HX-HY, TX-TY, NTX-NTY) :-
	\+ HX = TX,
	\+ HY = TY,
	NTX is TX + sign(HX-TX),
	NTY is TY + sign(HY-TY).

moveHead([_], NewHead, [NewHead]).
moveHead([_, NextStart | TailStart], NewHead, [NewHead, NewNext| NewTail]) :-
  (adjacent(NextStart, NewHead)
    *-> (NewNext = NextStart, NewTail = TailStart)
    ;
    (
      newTailPos(NewHead, NextStart, NewNext),
      moveHead([NextStart | TailStart], NewNext, [NewNext | NewTail])
    )).

applyMotion(motion(up, 1), [HX-HY | TailPos], NewPos) :-
  NHY is HY + 1,
  moveHead([HX-HY | TailPos], HX-NHY, NewPos).
applyMotion(motion(down, 1), [HX-HY | TailPos], NewPos) :-
  NHY is HY - 1,
  moveHead([HX-HY | TailPos], HX-NHY, NewPos).
applyMotion(motion(left, 1), [HX-HY | TailPos], NewPos) :-
  NHX is HX - 1,
  moveHead([HX-HY | TailPos], NHX-HY, NewPos).
applyMotion(motion(right, 1), [HX-HY | TailPos], NewPos) :-
  NHX is HX + 1,
  moveHead([HX-HY | TailPos], NHX-HY, NewPos).

part1(Motions) :-
  scanl(applyMotion, Motions, [0-0, 0-0], ReachedStates),
  setof(TailPosition, State^(member(State, ReachedStates), last(State, TailPosition)), TailPositions),
  length(TailPositions, NumberOfVisitedPositions),
  write(NumberOfVisitedPositions), nl.

part2(Motions) :-
  scanl(applyMotion, Motions, [0-0, 0-0, 0-0, 0-0, 0-0, 0-0, 0-0, 0-0, 0-0, 0-0], ReachedStates),
  setof(TailPosition, State^(member(State, ReachedStates), last(State, TailPosition)), TailPositions),
  length(TailPositions, NumberOfVisitedPositions),
  write(NumberOfVisitedPositions), nl.

main :-
  readFileChars('input.txt', Input),
  motions(CompressedMotions, Input, []),
  decompressMotions(CompressedMotions, Motions),
  part1(Motions),
  part2(Motions),
	halt.

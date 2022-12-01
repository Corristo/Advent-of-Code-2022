checkCharAndReadRest(10, [], _) :- !.
checkCharAndReadRest(-1, [], _) :- !.
checkCharAndReadRest(end_of_file, [], _) :-  !.
checkCharAndReadRest(Char, [Char|Chars], InStream) :-
  get_code(InStream, NextChar),
  checkCharAndReadRest(NextChar, Chars, InStream).

readLine(InStream, W) :-
  get_code(InStream, Char),
  checkCharAndReadRest(Char, Chars, InStream),
  atom_codes(W, Chars).

readStream(InStream, []) :-
  at_end_of_stream(InStream), !.
readStream(InStream, [H | T]) :-
  \+  at_end_of_stream(InStream),
  readLine(InStream, H),
  readStream(InStream, T).

readFile(FilePath, Lines) :-
  open(FilePath, read, Stream),
  readStream(Stream, Lines),
  close(Stream).

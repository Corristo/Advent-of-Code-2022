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

word([C|Cs]) --> [C], { \+ char_code(C, 10), \+ char_code(C, 32) }, !, word(Cs).
word([]) --> [].

dir(dir(Name, Contents)) -->
	['$', ' ', 'c', 'd', ' '], word(Name), ['\n'],
	['$', ' ', 'l', 's', '\n'],
	dirContents(Contents),
	( ['$', ' ', 'c', 'd', ' ', '.', '.', '\n'] ; [] ).

dirContents([file(Size, Name)|Contents]) -->
	num(Size), [' '], word(Name), ['\n'], !,
	dirContents(Contents).
dirContents(Contents) -->
	['d', 'i', 'r', ' '], word(_), ['\n'], !,
	dirContents(Contents).
dirContents([dir(Name, Contents)|OtherContents]) -->
	dir(dir(Name, Contents)), !,
	dirContents(OtherContents).
dirContents([]) --> [].

totalSize(file(Size, _), Size).
totalSize([], 0).
totalSize(dir(_, Contents), Size) :-
	maplist(totalSize, Contents, Sizes),
	sum_list(Sizes, Size).

sizeOfSmallDirs(file(_, _), 0).
sizeOfSmallDirs(dir(_, Cs), Size) :-
	totalSize(dir(_, Cs), TotalSize),
	maplist(sizeOfSmallDirs, Cs, SmallSizes),
	sum_list(SmallSizes, Size1),
	( (TotalSize =< 100000 -> Size is Size1 + TotalSize)
	; Size is Size1).

sizeOfDirectoryToDelete(RequiredSize, dir(_, Cs), ActualSize) :-
	totalSize(dir(_, Cs), TotalSize),
	TotalSize >= RequiredSize,
	(
		setof(Size, Dir^(member(Dir, Cs), sizeOfDirectoryToDelete(RequiredSize, Dir, Size)), Sizes)
			*-> (Sizes = [ActualSize|_])
		; ActualSize is TotalSize
	).

main :-
	readFileChars('input.txt', Input),
	dirContents([Root], Input, []),

	%% part 1
	sizeOfSmallDirs(Root, SumOfTotalSizesOfDirectoriesWithTotalSizeOfAtMost100k),
	write(SumOfTotalSizesOfDirectoriesWithTotalSizeOfAtMost100k), nl,

	%% part 2
	totalSize(Root, UsedDiskSpace),
	MinimumSizeNeededToBeFreed is 30000000 - (70000000 - UsedDiskSpace),
	sizeOfDirectoryToDelete(MinimumSizeNeededToBeFreed, Root, SizeOfDirToDelete),
	write(SizeOfDirToDelete), nl.

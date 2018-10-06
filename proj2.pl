% Project 2 - Fillin Puzzles solver
% COMP30020 Declarative Programming, 2018 Semester 2
% The University of Melbourne, School of Computing and Information Systems
% 
% A program for solving fill-it-in style puzzles. The program takes a list 
% of words and an empty or incomplete puzzle file and generates a solution.
%
% Author: Adam Quigley
% Date created: 30.09.2018

:- ensure_loaded(library(clpfd)).

main(PuzzleFile, WordlistFile, SolutionFile) :-
	read_file(PuzzleFile, PuzzleCharList),
	read_file(WordlistFile, Wordlist),
	valid_puzzle(PuzzleCharList),
	create_free_variables(PuzzleCharList, Puzzle),
	solve_puzzle(Puzzle, Wordlist, Solved),
	print_puzzle(SolutionFile, Solved).

valid_puzzle([]).
valid_puzzle([Row|Rows]) :-
	maplist(same_length(Row), Rows).

read_file(Filename, Content) :-
	open(Filename, read, Stream),
	read_lines(Stream, Content),
	close(Stream).

read_lines(Stream, Content) :-
	read_line(Stream, Line, Last),
	(   Last = true
	->  (   Line = []
	    ->  Content = []
	    ;   Content = [Line]
	    )
	;  Content = [Line|Content1],
	    read_lines(Stream, Content1)
	).

read_line(Stream, Line, Last) :-
	get_char(Stream, Char),
	(   Char = end_of_file
	->  Line = [],
	    Last = true
	; Char = '\n'
	->  Line = [],
	    Last = false
	;   Line = [Char|Line1],
	    read_line(Stream, Line1, Last)
	).

print_puzzle(SolutionFile, Puzzle) :-
	open(SolutionFile, write, Stream),
	maplist(print_row(Stream), Puzzle),
	close(Stream).

print_row(Stream, Row) :-
	maplist(put_puzzle_char(Stream), Row),
	nl(Stream).

put_puzzle_char(Stream, Char) :-
	(   var(Char)
	->  put_char(Stream, '_')
	;   put_char(Stream, Char)
	).

% should hold when Puzzle is a solved version of Puzzle0, with the
% empty slots filled in with words from WordList.  Puzzle0 and Puzzle
% should be lists of lists of characters (single-character atoms), one
% list per puzzle row.  WordList is also a list of lists of
% characters, one list per word.

solve_puzzle(Puzzle, _, Puzzle).

% replace any underscore character with a free variable
logical_variable(Char, Result) :-
	(	Char = '_'
	->	Result = _
	;	Result = Char
	).

% convert all underscore chars in a row to logical variables
map_row_vars(Row, NewRow) :-
	maplist(logical_variable, Row, ResultRow),
	NewRow = ResultRow.

% converts all underscore chars in a puzzle to free variables
% the free variables will be unified with letters to solve the puzzle
create_free_variables(Puzzle, FreeVariablePuzzle) :-
	maplist(map_row_vars, Puzzle, ResultPuzzle),
	FreeVariablePuzzle = ResultPuzzle.

% find slots inside puzzle rows / columns
% a slot is a sequence of non-solid squares greater than one in length
% if free var is found, add to Accumulator
% if non-free var found but none accumalted, keep searching
% if non-free var found but only one var accumulated, reset accum
%	and keep searching. Slots must have a length greater than one
% if non-free var found and some accumulated, add slot to Slots and reset Accum
get_slots([], Accum, AccumList, Slots) :-
	(	length(Accum, 0)
	->	Slots = AccumList
	;	length(Accum, 1)
	->	Slots = AccumList	
	;	NewAccumList = [Accum | AccumList],
		Slots = NewAccumList
	).
get_slots([H|T], Accum, AccumList, Slots) :-
	(	var(H)
	->	get_slots(T, [H|Accum], AccumList, Slots)
	;	length(Accum, 0)
	->	get_slots(T, Accum, AccumList, Slots)
	;	length(Accum, 1)
	->	get_slots(T, [], AccumList, Slots)
	;	NewAccumList = [Accum | AccumList],
		get_slots(T, [], NewAccumList, Slots)
	).
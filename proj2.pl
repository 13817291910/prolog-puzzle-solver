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
	find_all_puzzle_slots(Puzzle, Slots),
	solve_puzzle(Puzzle, Slots, Wordlist, Solved),
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

solve_puzzle(Puzzle, _, _, Puzzle).

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

% find slots inside a puzzles row or column
% a slot is a sequence of non-solid squares greater than one in length
% if free var or non '#' character is found, add to Accumulator
% if non-free var found but none accumalted, keep searching
% if non-free var found but only one var accumulated, reset accum
%	and keep searching. Slots must have a length greater than one
% if non-free var found and some accumulated, add slot to Slots and reset Accum
get_row_slots([], Accum, AccumList, Slots) :-
	(	length(Accum, 0)
	->	Slots = AccumList
	;	length(Accum, 1)
	->	Slots = AccumList	
	;	NewAccumList = [Accum | AccumList],
		Slots = NewAccumList
	).
get_row_slots([H|T], Accum, AccumList, Slots) :-
	(	var(H)
	->	get_row_slots(T, [H|Accum], AccumList, Slots)
	;	H \= '#'
	->	get_row_slots(T, [H|Accum], AccumList, Slots)
	;	length(Accum, 0)
	->	get_row_slots(T, Accum, AccumList, Slots)
	;	length(Accum, 1)
	->	get_row_slots(T, [], AccumList, Slots)
	;	NewAccumList = [Accum | AccumList],
		get_row_slots(T, [], NewAccumList, Slots)
	).

% finds all the slots within a given puzzle transposition
% rows are reversed because get_row_slots constructs lists backwards
get_all_slots([], Accum, Accum).
get_all_slots([H|T], Accum, AllSlots) :-
	reverse(H, Row),
	get_row_slots(Row, [], [], RowSlots),
	append(Accum, RowSlots, AccumList),
	get_all_slots(T, AccumList, AllSlots).

% finds all the slots in a puzzle by looking at both rows and cols
% the puzzle must be a valid puzzle otherwise transpose will fail
find_all_puzzle_slots(RowPuzzle, AllSlots) :-
	get_all_slots(RowPuzzle, [], RowSlots),
	transpose(RowPuzzle, ColPuzzle),
	get_all_slots(ColPuzzle, [], ColSlots),
	append(RowSlots, [], Accum),
	append(ColSlots, Accum, AllSlots).

% return the numbers of free variables in a list
% e.g. free_var_length([X, a, Y], L) -> L = 2
free_var_length([],0).
free_var_length([H|T], L) :-
	( 	var(H)
	-> free_var_length(T, L1),
		L is L1 + 1
	;	free_var_length(T, L)
	).

% finds the first slot in a list of slots with the highest number
% of un-unified variables to be next considered
get_max_fillable_slot([], CurrSlot, CurrSlot).
get_max_fillable_slot([NextSlot|T], CurrSlot, MaxSlot) :-
	(	more_fillable(CurrSlot, NextSlot)
	->	get_max_fillable_slot(T, NextSlot, MaxSlot)
	;	get_max_fillable_slot(T, CurrSlot, MaxSlot)
	).

% true if a slot is more fillable that the currently considered slot
% meaning there are more free variables to be unified
more_fillable(CurrMaxSlot, NextSlot) :-
	free_var_length(CurrMaxSlot, L1),
	free_var_length(NextSlot, L2),
	L2 > L1.

% evaluates true if all the terms in a slot match the corresponding characters
% in a Word, where a word is a list of characters e.g. ['c','a','t']
% assumes that the word and slot are of equal length
chars_match([], _).
chars_match([SlotHead|SlotTail], [WordHead|WordTail]) :-
	(	var(SlotHead)
	->	chars_match(SlotTail, WordTail)
	;	SlotHead = WordHead,
		chars_match(SlotTail, WordTail)
	).

% evaluates true if a Slot can be unified with a word
% each term in a slot must match a character's position in the word
are_unifiable(Slot, Word) :-
	same_length(Slot, Word),
	chars_match(Slot, Word).
	
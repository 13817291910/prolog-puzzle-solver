% Project 2 - Fillin Puzzles solver
% COMP30020 Declarative Programming, 2018 Semester 2
% The University of Melbourne, School of Computing and Information Systems
% 
% A program for solving fill-it-in style puzzles. The program takes a list 
% of words and an empty or incomplete puzzle file and generates a solution.
% The solution is then written to a separate solution file.
%
% Author: Adam Quigley
% Date created: 30.09.2018

:- ensure_loaded(library(clpfd)).

% Reads a puzzle file and a file containing a list of words and
% solves the puzzle by creating a representation of the puzzle as 'slots',
% and unifying the slots with words from the word list.
% The solution is then printed to a solution file.
main(PuzzleFile, WordlistFile, SolutionFile) :-
	read_file(PuzzleFile, PuzzleCharList),
	read_file(WordlistFile, WordList),
	valid_puzzle(PuzzleCharList),
	create_free_variables(PuzzleCharList, Puzzle),
	find_all_puzzle_slots(Puzzle, Slots),
	solve_puzzle(Puzzle, Slots, WordList, Solved),
	print_puzzle(SolutionFile, Solved).

% A puzzle is valid when all rows are the same length
valid_puzzle([]).
valid_puzzle([Row|Rows]) :-
	maplist(same_length(Row), Rows).

% Reads a file and into a variable Content
read_file(Filename, Content) :-
	open(Filename, read, Stream),
	read_lines(Stream, Content),
	close(Stream).

% Reads a stream of lines from a file into a variable Content
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

% Reads individual characters from an input stream into a variable Line
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

% Writes characters in a Puzzle solution into a solution file
print_puzzle(SolutionFile, Puzzle) :-
	open(SolutionFile, write, Stream),
	maplist(print_row(Stream), Puzzle),
	close(Stream).

% Writes an individual row from a solution puzzle into an IO stream
print_row(Stream, Row) :-
	maplist(put_puzzle_char(Stream), Row),
	nl(Stream).

% Writes an individual character into an IO stream
put_puzzle_char(Stream, Char) :-
	(   var(Char)
	->  put_char(Stream, '_')
	;   put_char(Stream, Char)
	).

% Solves a puzzle represented as a list of rows by repeatedly finding
% words and unifying them with slots until the list of words is empty
solve_puzzle(Puzzle, _, [], Puzzle).
solve_puzzle(Puzzle, Slots, WordList, Solved) :-
	get_max_fillable_slot(Slots, [], MaxSlot),
	get_unifiable_word(WordList, MaxSlot, Word),
	MaxSlot = Word,
	delete(WordList, Word, NewWordList),
	solve_puzzle(Puzzle, Slots, NewWordList, Solved).

% Replace any underscore character with a free variable
logical_variable(Char, Result) :-
	(	Char = '_'
	->	Result = _
	;	Result = Char
	).

% Convert all underscore chars in a row to logical / free variables
map_row_vars(Row, NewRow) :-
	maplist(logical_variable, Row, ResultRow),
	NewRow = ResultRow.

% Converts all underscore chars in a puzzle to free variables
% the free variables will be unified with letters to solve the puzzle
create_free_variables(Puzzle, FreeVariablePuzzle) :-
	maplist(map_row_vars, Puzzle, ResultPuzzle),
	FreeVariablePuzzle = ResultPuzzle.

% Find slots inside a puzzles row or column.
% A slot is a sequence of non-solid squares greater than one in length.
% If free var or non '#' character is found, add to accumulator.
% If non-free var found but none accumulated, keep searching.
% If non-free var found but only one var accumulated, reset accum
%	and keep searching. Slots must have a length greater than one.
% If non-free var found and some accumulated, add Slot and reset Accum.
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

% Finds all the slots within a given puzzle transposition.
% Rows are reversed because get_row_slots constructs lists backwards.
get_all_slots([], Accum, Accum).
get_all_slots([H|T], Accum, AllSlots) :-
	reverse(H, Row),
	get_row_slots(Row, [], [], RowSlots),
	append(Accum, RowSlots, AccumList),
	get_all_slots(T, AccumList, AllSlots).

% Finds all the slots in a puzzle, looking at both rows and columns.
% The puzzle must be a valid puzzle otherwise transpose will fail.
find_all_puzzle_slots(RowPuzzle, AllSlots) :-
	get_all_slots(RowPuzzle, [], RowSlots),
	transpose(RowPuzzle, ColPuzzle),
	get_all_slots(ColPuzzle, [], ColSlots),
	append(RowSlots, [], Accum),
	append(ColSlots, Accum, AllSlots).

% Return the numbers of free variables in a list
% e.g. free_var_length([X, a, Y], L) -> L = 2
free_var_length([],0).
free_var_length([H|T], L) :-
	( 	var(H)
	-> free_var_length(T, L1),
		L is L1 + 1
	;	free_var_length(T, L)
	).

% Finds the first slot in a list of slots with the highest number
% of un-unified variables to be next considered.
get_max_fillable_slot([], CurrSlot, CurrSlot).
get_max_fillable_slot([NextSlot|T], CurrSlot, MaxSlot) :-
	(	more_fillable(CurrSlot, NextSlot)
	->	get_max_fillable_slot(T, NextSlot, MaxSlot)
	;	get_max_fillable_slot(T, CurrSlot, MaxSlot)
	).

% True if a slot is more fillable that the currently considered slot,
% meaning there are more free variables to be unified.
more_fillable(CurrMaxSlot, NextSlot) :-
	free_var_length(CurrMaxSlot, L1),
	free_var_length(NextSlot, L2),
	L2 > L1.

% True if all the terms in a slot match the corresponding characters
% in a Word, where a word is a list of chars e.g. ['c','a','t']
% Assumes that the word and slot are of equal length.
chars_match([], _).
chars_match([SlotHead|SlotTail], [WordHead|WordTail]) :-
	(	var(SlotHead)
	->	chars_match(SlotTail, WordTail)
	;	SlotHead = WordHead,
		chars_match(SlotTail, WordTail)
	).

% True if a Slot can be unified with a word.
% Each term in a slot must match a character's position in the word.
are_unifiable(Slot, Word) :-
	same_length(Slot, Word),
	chars_match(Slot, Word).
	
% Finds the first word in a list of words that is able to be unified 
% with a given slot and binds it to Word.
get_unifiable_word([], _, []).
get_unifiable_word([WordsHead|WordsTail], Slot, Word) :-
	(	are_unifiable(Slot, WordsHead)
	->	Word = WordsHead
	;	get_unifiable_word(WordsTail, Slot, Word)
	).
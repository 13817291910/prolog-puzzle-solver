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
	read_file(PuzzleFile, Puzzle),
	read_file(WordlistFile, Wordlist),
	valid_puzzle(Puzzle),
	solve_puzzle(Puzzle, Wordlist, Solved),
	print_puzzle(SolutionFile, Solved).

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

valid_puzzle([]).
valid_puzzle([Row|Rows]) :-
	maplist(same_length(Row), Rows).


% should hold when Puzzle is a solved version of Puzzle0, with the
% empty slots filled in with words from WordList.  Puzzle0 and Puzzle
% should be lists of lists of characters (single-character atoms), one
% list per puzzle row.  WordList is also a list of lists of
% characters, one list per word.

%solve_puzzle(Puzzle, _, Puzzle).

solve_puzzle(Puzzle0, WordList, Puzzle) :-
	Puzzle = Puzzle0.


% Holds when the a slot exists to be filled in the puzzle.
% Slot will be a maximal horizontal or vertical sequence of 
% fill-able and pre-filled squares within the puzzle.
select_slot(Puzzle, Slot) :-
	length(Puzzle, L),
	Slot = L.


% Filters a slot to give a list of characters already filled
get_filled_chars(SlotList, FilledCharList) :-
    include(is_alpha, SlotList, FilledCharList).

% 
%get_valid_words(Slot, Wordlist, ValidWordList) :-
%	get_filled_chars(Slot, FilledCharList),
%	subset(FilledCharList, WordList)
%% also test for length.

% bind_terms([], _, []).
% bind_terms(_, [], []).
% bind_terms(LogicalVariables, TermList, BoundTermList) :-
%  	(LogicalVariables, TermList, x).
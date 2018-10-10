% Module: proj2
% Author: Adam Quigley
% Last updated: 09.10.2018
%
% Program for solving Fill-In style puzzles
% COMP30020 Declarative Programming, 2018 Semester 2, Project 2
% The University of Melbourne, School of Computing and Information Systems
% 
% This module solves Fill-In (fill-it-in) style puzzles by constructing a
% representation of the puzzle at a list of 'slots', and repeatedly
% trying valid words from the provided word list until it finds a 
% solution. Once found, the solution is written to a solution file.
% 
% The program reads in a puzzle file and creates a list of 'slots' by 
% aggregating series of non-black ('#') squares of length greater than one. 
% It sorts the list of slots and the list of words, and then proceeds to 
% match words with slots. If there is a word that uniquely matches a slot 
% it will be immediately unified with that slot, otherwise the largest slot
% is selected and unified with valid  matching word. 
% To narrow the possible search space, the program sorts the slots by 
% number of possible unifications with words, then by length. 
% This ensures that the longest words with the fewest possible matches 
% amongst words of their size are picked first. If no solution is found, 
% the program backtracks and tries different combinations of words 
% and slots until the puzzle is solved.
% 
% Usage:
% Call program with main(PuzzleFile, WordlistFile, SolutionFile) where:
% PuzzleFile    - the dir location of a valid puzzle file
% WordlistFile  - the dir location of a list of words
% SolutionFile  - the dir location of a file to write a solution to

:- ensure_loaded(library(clpfd)).
:- use_module(library(pairs)).

% Reads a puzzle file and a file containing a list of words and
% solves the puzzle by creating a representation of the puzzle as 'slots'.
% It repeatedly matches slots with words until a solution is found.
% The solution is then printed to a solution file.
main(PuzzleFile, WordlistFile, SolutionFile) :-
	read_file(PuzzleFile, PuzzleCharList),
	read_file(WordlistFile, RawWordList),
	valid_puzzle(PuzzleCharList),
	create_free_variables(PuzzleCharList, Puzzle),
	find_all_puzzle_slots(Puzzle, UnsortedSlots),
	sort_by_length_desc(UnsortedSlots, Slots),
	!,
	remove_filled_words(Slots, RawWordList, WordList),
	remove_filled_slots(Slots, UnfilledSlots),
	sort_by_length_desc(WordList, SortedWordList),
	!,
	solve_puzzle(Puzzle, UnfilledSlots, SortedWordList, Solved),
	print_puzzle(SolutionFile, Solved).

% A puzzle is valid when all rows are the same length
valid_puzzle([]).
valid_puzzle([Row|Rows]) :-
	maplist(same_length(Row), Rows).

% Sorts a list of lists in descending order by sublist length
sort_by_length_desc(List, ByLength) :-
	map_list_to_pairs(length, List, Pairs),
	sort(1, @>=, Pairs, Sorted),
	pairs_values(Sorted, ByLength).

% Sorts a list of slots from least possible unifications to most
sort_by_least_unifiable_words(Slots, WordList, ByNumUnifiableWords) :-
	map_list_to_pairs(num_of_unifiable_words(WordList), Slots, Pairs),
	sort(1, @=<, Pairs, Sorted),
	pairs_values(Sorted, ByNumUnifiableWords).

% Takes a list of slots and a list of sords and deletes any filled slots
% from the word list. This is important because we don't need to search
% for a solution to a slot that is already answered.
remove_filled_words([], WordList, WordList).
remove_filled_words([SlotHead|T], InputWordList, ResultWordList) :-
	(	is_filled_slot(SlotHead)
	->	delete(InputWordList, SlotHead, RemainingWordList),
		remove_filled_words(T, RemainingWordList, ResultWordList)
	;	remove_filled_words(T, InputWordList, ResultWordList)
	).

% Remove all slots that have been filled in from a list of slots
remove_filled_slots(InputSlots, OutputSlots) :-
    exclude(is_filled_slot, InputSlots, OutputSlots).

% Solves a puzzle represented as a list of rows by repeatedly finding
% words and unifying them with slots until the list of words is empty.
% Slot that are filled as a by-product of unifications must be removed.
% Slots are sorted by the number of unifiable words, then by length desc.
% Both are stable sorts. This ensures that the longest slots are tried
% first, and the slot with the minimum matches tried.
solve_puzzle(Puzzle, _, [], Puzzle).
solve_puzzle(Puzzle, Slots, WordList, Solved) :-
	choose_slots_and_word(Slots, WordList, BestSlot, BestWord),
	BestSlot = BestWord,
	delete(WordList, BestWord, NewWordList),
	remove_filled_words(Slots, NewWordList, UnfilledWordList),
	remove_filled_slots(Slots, UnfilledSlots),
	sort_by_least_unifiable_words(UnfilledSlots, WordList, ByUnifiableWords),
	sort_by_length_desc(ByUnifiableWords, ResortedSlots),
	solve_puzzle(Puzzle, ResortedSlots, UnfilledWordList, Solved).

% True for all words that can unify with a given slot
unifiable_with_slot(Slot, WordList, Word) :-
	member(Word, WordList),
  	are_unifiable(Slot, Word).

% Collects all words that are unifiable with a slot into a list
all_unifiable_words(Slot, WordList, UnifiableWords)  :- 
	findall(Word, unifiable_with_slot(Slot, WordList, Word), UnifiableWords).

% Finds the number of words that can unify with a particular slot
num_of_unifiable_words(Slot, WordList, N) :-
	all_unifiable_words(Slot, WordList, UnifiableWords),
	length(UnifiableWords, N).

% If there's a word of a unique length, immediately unify it with
% the single slot that matches its length.
% Otheriwse choose first (longest) slot and find a unifiable word.
choose_slots_and_word([SlotHead|ST], [WH|WT], BestSlot, BestWord) :-
	(	get_unique_word([WH|WT], [WH|WT], UniqueWord)
	->	BestWord = UniqueWord,
		length(BestWord, L),
		get_slot_of_length([SlotHead|ST], L, MatchingLenSlot),
		BestSlot = MatchingLenSlot
	;	BestSlot = SlotHead,
		unifiable_with_slot(SlotHead, [WH|WT], BestWord)
	).

% True if a word of unique length exists in a list of words
get_unique_word(WordList, [WordListHead|T], Word) :-
(	is_unique_word(WordList, WordListHead)
->	Word = WordListHead	
;	get_unique_word(WordList, T, Word)
).

% True if a word is the only word of its length in a list of words
is_unique_word(WordList, Word) :-
	length(Word, WordLen),
	count_words_of_length(WordList, WordLen, Count),
	Count is 1.

% Finds the number of words in a list of words that match a given length
count_words_of_length([], _, 0).
count_words_of_length([WordListHead|T], WordLen, C) :-
(	length(WordListHead, WordLen)
->	count_words_of_length(T, WordLen, C1),
	C is C1 + 1
;	count_words_of_length(T, WordLen, C)
).

% Unifies 'Slot' with a slot in a list of Slots if it's length matches Len
get_slot_of_length([SlotsHead|T], Len, Slot) :-
(	length(SlotsHead, Len)
-> 	Slot = SlotsHead
;	get_slot_of_length(T, Len, Slot)
).

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

% True if a slot has been unified, ie. [a,b,c]
is_filled_slot([]).
is_filled_slot([H|T]) :-
	not(var(H)),
	is_filled_slot(T).

%%%%%%%%%%%%%%%%%%%%%% SLOT CONSTRUCTION PREDICATES %%%%%%%%%%%%%%%%%%%%%% 

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% I/O PREDICATES %%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

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
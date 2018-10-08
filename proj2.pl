% Project 2 - Fillin Puzzles solver
% A program for solving fill-it-in style puzzles
% COMP30020 Declarative Programming, 2018 Semester 2
% The University of Melbourne, School of Computing and Information Systems
% 
% The program takes a list of words and an empty or incomplete puzzle 
% file and generates a solution. The solution is then written to a 
% separate solution file.
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
	read_file(WordlistFile, RawWordList),
	valid_puzzle(PuzzleCharList),
	create_free_variables(PuzzleCharList, Puzzle),
	find_all_puzzle_slots(Puzzle, Slots),
	remove_filled_words(Slots, RawWordList, WordList),
	solve_puzzle(Puzzle, Slots, WordList, Solved),
	!,
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
% words and unifying them with slots until the list of words is empty.
% Slot that are filled as a by-product of unifications must also
% be removed.
solve_puzzle(Puzzle, _, [], Puzzle).
solve_puzzle(Puzzle, Slots, WordList, Solved) :-
	choose_slots_and_word(Slots, WordList, BestSlot, BestWord),
	BestSlot = BestWord,
	delete(WordList, BestWord, NewWordList),
	remove_filled_words(Slots, NewWordList, UnfilledWordList),
	remove_filled_slots(Slots, UnfilledSlots),
	solve_puzzle(Puzzle, UnfilledSlots, UnfilledWordList, Solved).

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

% Takes a list of Slots and a list of Words and deletes any filled Slots
% from the Word list. This is important because we don't need to search
% for a solution to a slot that is already answered.
remove_filled_words([], WordList, WordList).
remove_filled_words([SlotHead|T], InputWordList, ResultWordList) :-
	(	is_filled_slot(SlotHead)
	->	delete(InputWordList, SlotHead, RemainingWordList),
		remove_filled_words(T, RemainingWordList, ResultWordList)
	;	remove_filled_words(T, InputWordList, ResultWordList)
	).

% Remove all Slots that have been filled in from a list of slots
remove_filled_slots(InputSlots, OutputSlots) :-
    exclude(is_filled_slot, InputSlots, OutputSlots).

% Returns the number of filled terms in a list
% e.g. [X,Y,a,b] -> L = 2
filled_term_length([],0).
filled_term_length([H|T], L) :-
	( 	not(var(H))
	-> filled_term_length(T, L1),
		L is L1 + 1
	;	filled_term_length(T, L)
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
	not(is_filled_slot(NextSlot)),
	filled_term_length(CurrMaxSlot, TermLength1),
	filled_term_length(NextSlot, TermLength2),
	TermLength2 >= TermLength1,
	length(CurrMaxSlot, SlotLength1),
	length(NextSlot, SlotLength2),
	SlotLength2 >= SlotLength1.

% True if a slot has been unified, ie. [a,b,c]
is_filled_slot([]).
is_filled_slot([H|T]) :-
	not(var(H)),
	is_filled_slot(T).

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

% Accumulates a list of all slots that are unifiable with a word
get_all_unifiable_slots([], _, Slots, Slots).
get_all_unifiable_slots([SlotHead|T], Word, Accum, UnifiableSlots) :-
	(	are_unifiable(SlotHead, Word)
	->  get_all_unifiable_slots(T, Word, [SlotHead|Accum], UnifiableSlots)
	;	get_all_unifiable_slots(T, Word, Accum, UnifiableSlots)
	).

% True if a word can only be unified with a single slot, because of
% matching terms within the slot. If true, Slot binds to the single
% unifiable slot in the list of Slots.
unifiable_with_one_slot(Slots, Word, Slot) :-
	get_all_unifiable_slots(Slots, Word, [], UnifiableSlots),
	(	length(UnifiableSlots, 1)
	->	UnifiableSlots = [H|_],
		Slot = H
	).

% Finds a words in a word list that can be uniquely unified 
% by a single slot each
find_single_unifiable_words([], _, Words, Words).
find_single_unifiable_words([WH|WT], Slots, Accum, Words) :-
	(	unifiable_with_one_slot(Slots, WH, _)
	->	find_single_unifiable_words(WT, Slots, [WH|Accum], Words)
	;	find_single_unifiable_words(WT, Slots, Accum, Words)
	).

% True if there are one or more words that match only a single slot in 
% the list of slots. Binds Word and Slot to the first word found if so
get_only_matching_word_and_slot([WordListH|WT],[SlotsH|ST], Word, Slot) :-
	find_single_unifiable_words([WordListH|WT],[SlotsH|ST], [], MatchWords),
	length(MatchWords, L),
	(	L > 0
	->	MatchWords = [MatchWordH|_],
		Word = MatchWordH,
		get_all_unifiable_slots([SlotsH|ST], Word,[], UnifiableSlots),
		get_most_matching_slot(UnifiableSlots, [], Slot)
	).
	 
% Unifies 'Slot' with a slot in a list of Slots if it's length matches Len
get_slot_of_length([SlotsHead|T], Len, Slot) :-
	(	length(SlotsHead, Len)
	-> 	Slot = SlotsHead
	;	get_slot_of_length(T, Len, Slot)
	).

% Finds the slot that is a the best match for a word
% Assumes that all slots are known to be unifiable with a word. The most
% matching slot if the slot with the most prefilled terms
% e.g. Slot [X,a,Z] matches Word [c,a,t] better than Slot [A,B,C]
get_most_matching_slot([], Slot, Slot).
get_most_matching_slot([SlotHead|T], PrevSlot, Slot) :-
	filled_term_length(SlotHead, CurrSlotLen),
	filled_term_length(PrevSlot, PrevSlotLen),
	(	CurrSlotLen >= PrevSlotLen
	->	get_most_matching_slot(T, SlotHead, Slot)
	;	get_most_matching_slot(T, PrevSlot, Slot)
	).

% Gets a unique word from a list. Word = [] if no unique word exists
get_unique_word(WordList, [WordListHead|T], Word) :-
	(	is_unique_word(WordList, WordListHead)
	->	Word = WordListHead	
	;	get_unique_word(WordList, T, Word)
	).

% True if a Word is the only word of it's length in a list of words
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

% Finds the first unique word in a list of words, or returns the first
% word in the word list if no unique options exist.
get_next_best_word([WordListHead|T], Word) :-
	get_unique_word([WordListHead|T], [WordListHead|T], UniqueWord),
	(	length(UniqueWord, L),
		L is 0
	->	Word = [],
		!
	;	Word = UniqueWord
	).

% If there's a word that can only be matched with one slot because it's 
% 	length is unique in the words list, pick that.
% If not, see if any slot exists that can only be unified with one word.
% Otherwise choose the slot that reduces further choices that most and
% 	find a word in the Words list that it unifies with.
choose_slots_and_word([SH|ST], [WH|WT], BestSlot, BestWord) :-
	(	get_unique_word([WH|WT], [WH|WT], UniqueWord)
	->	BestWord = UniqueWord,
		length(BestWord, L),
		get_slot_of_length([SH|ST], L, MatchingLenSlot),
		BestSlot = MatchingLenSlot
	;	(	get_only_matching_word_and_slot([WH|WT],[SH|ST], 
											MatchWord, MatchSlot)
		->	BestWord = MatchWord,
			BestSlot = MatchSlot
		;	get_max_fillable_slot([SH|ST], [], MaxSlot),
			get_unifiable_word([WH|WT], MaxSlot, UnifiableWord),
			BestSlot = MaxSlot,
			BestWord = UnifiableWord
		)
	).
# Fill-In Puzzles - Prolog Project 2

## The University of Melbourne, School of Computing and Information Systems
## COMP30020 Declarative Programming, 2018 Semester 2

This program solves [Fill-In (fill-it-in) style puzzles](https://en.wikipedia.org/wiki/Fill-In_(puzzle)) by constructing a representation of the puzzle at a list of 'slots', and repeatedly trying valid words from the provided word list until it finds a  solution. Once found, the solution is written to a solution file.

## Strategy
The program reads in a puzzle file and creates a list of 'slots' by aggregating series of non-black ('#') squares of length greater than one. It sorts the list of slots and the list of words, and then proceeds to match words with slots. If there is a word that uniquely matches a slot it will be immediately unified with that slot, otherwise the largest slot is selected and unified with valid  matching word.  To narrow the possible search space, the program sorts the slots by number of possible unifications with words, then by length. This ensures that the longest words with the fewest possible matches amongst words of their size are picked first. If no solution is found, the program backtracks and tries different combinations of words and slots until the puzzle is solved.

## Usage

```
[proj2].
main('tests/puzzle7', 'tests/words7', 'tests/answer.txt').
```
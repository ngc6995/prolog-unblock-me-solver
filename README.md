## Unblock Me Puzzle Solver

Recently, I am relearning Prolog, I never use this language to write a practical program before. This is the first one.

### Commands to operate a puzzle.

> Input a block.

A block is represented as `Name/Form/Length/Row/Column.`

- `Name`   : name of a block(a, b, c ...)
  - x is reserved for the target block.
  - x is horizontal, length 2 and place at row 4.
- `Form`   : horizontal(h) or vertical(v).
- `Length` : length of a block(2 or 3).
- `Row`    : 1 to 6 from bottom to top.
- `Column` : 1 to 6 from left to right.

For example, all the blocks of this puzzle.
```
6 F F F . . G
5 . . D . . G
4 X X D . . G =>
3 C . D . E E
2 C . . . B .
1 A A A . B .
  1 2 3 4 5 6

a/h/3/1/1.   b/v/2/1/5.   c/v/2/2/1.
d/v/3/3/3.   e/h/2/3/5.   f/h/3/6/1.
g/v/3/4/6.   x/h/2/4/1.
``` 
> show.

Display puzzle.

> clear.

Clear puzzle.

> clear(X).

Clear a block.

> example.

An example puzzle.

> solve.

Solve this puzzle.

> help.

Show instructions.

>exit.

Quit the program.

All the above commands are a Prolog term, don't forget
the full stop before hit the Enter key.

### Run the program.

Start SWI-Prolog 8.4.3. and consult unblock_me.pl
```
Unblock Me Puzzle Solver
------------------------
help. for instructions

     6 . . . . . .
     5 . . . . . .
     4 . . . . . . =>
     3 . . . . . .
     2 . . . . . .
     1 . . . . . .
       1 2 3 4 5 6
 ] 
``` 
We can input a puzzle now ...

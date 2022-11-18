/*
	Unblock Me Puzzle Solver 1.8
	Test under SWI-Prolog 8.4.3
*/

:- dynamic puzzle/1.

/* --- User interface ------------------------------------------------------ */
unblock_me :-
	nl,
	write('Unblock Me Puzzle Solver'), nl,
	write('------------------------'), nl,
	write('help. for instructions'), nl,
	prompt(_, ''),
	assert(puzzle([])),
	print_puzzle([]),
	command_loop(run),
	retract(puzzle(_)).

command_loop(exit).
command_loop(_) :-
	write(' ] '),
	read(Command),
	do_this(Command),
	command_loop(Command).

do_this(N/F/L/R/C) :-
	\+ valid_format(N/F/L/R/C),
	write('Invalid format!'), nl, !.
do_this(_/F/L/R/C) :-
	out_of_edge(F/L/R/C),
	write('Block out of edge!'), nl,!.
do_this(N/_/_/_/_) :-
	exist(N),
	write('Duplicate block!'), nl, !.
do_this(_/F/L/R/C) :-
	overlap(F/L/R/C),
	write('Block overlap!'), nl, !.
do_this(N/F/L/R/C) :-
	place_block(N/F/L/R/C),
	puzzle(P),
	print_puzzle(P), !.

do_this(show) :- puzzle(P), print_puzzle(P), !.

do_this(clear) :-
	retract(puzzle(_)),
	assert(puzzle([])),
	print_puzzle([]), !.

do_this(clear(N)) :-
	puzzle(P),
	\+ member(N/_/_/_/_, P),
	write('Block not exist!'), nl, !.
do_this(clear(N)) :-
	puzzle(P),
	member(N/F/L/R/C, P),
	delete(P, N/F/L/R/C, P1),
	retract(puzzle(_)),
	assert(puzzle(P1)),
	print_puzzle(P1), !.

do_this(example) :-
	retract(puzzle(_)),
	assert(puzzle([a/h/3/1/1,b/v/2/1/4,c/v/2/2/3,d/h/2/3/4,e/v/2/2/6,f/v/3/4/2,g/v/3/4/5,h/v/2/4/6,x/h/2/4/3])),
	puzzle(P),
	print_puzzle(P), !.

do_this(solve) :-
	puzzle(P),
	\+ member(x/_/_/_/_, P),
	write('Target block not exist!'), nl, !.
do_this(solve) :-
	puzzle(P),
	\+ member(x/h/_/_/_, P),
	write('Target block must be horizontal!'), nl, !.
do_this(solve) :-
	puzzle(P),
	\+ member(x/h/2/_/_, P),
	write('Target block''s length must be equal to 2!'), nl, !.
do_this(solve) :-
	puzzle(P),
	\+ member(x/h/2/4/_, P),
	write('Target block must be place at row 4!'), nl, !.
do_this(solve) :-
	puzzle(P),
	nl, write('Solving ... '),
	get_time(Start),
	solve(P, Solution),
	get_time(Stop),
	ExecTime is Stop - Start,
	format('~2fs~n', [ExecTime]),
	length(P, TotalBlocks),
	format('Total ~d blocks.~n', [TotalBlocks]),
	length(Solution, TotalSteps),
	format('~d steps to solve this puzzle.~n', [TotalSteps]),
	write('Press space bar for next step.'), nl,
	print_puzzle(P),
	write('  <<< Initial State >>>'), nl,
	print_solution(Solution, TotalSteps), !.

do_this(help) :-
	nl,
	write('N/F/L/R/C. - place a block.'), nl,
	write('    Name   : name of a block(a, b, c ...)'), nl,
	write('             x is reserved for the target block.'), nl,
	write('             x is horizontal, length 2 and place'), nl,
	write('             at row 4(e.g. x/h/2/4/3).'), nl,
	write('    Form   : horizontal(h) or vertical(v).'), nl,
	write('    Length : length of a block(2 or 3).'), nl,
	write('    Row    : 1 to 6 from bottom to top.'), nl,
	write('    Column : 1 to 6 from left to right.'), nl,
	write('show.      - Display puzzle.'), nl,
	write('clear.     - Clear puzzle.'), nl,
	write('clear(X).  - Clear a block.'), nl,
	write('example.   - An example puzzle.'), nl,
	write('solve.     - Solve this puzzle.'), nl,
	write('help.      - Show this instructions.'), nl,
	write('exit.      - Quit the program.'),nl, !.

do_this(exit).

do_this(_) :- write('Unknown command!'), nl.

valid_format(N/F/L/R/C) :-
	atom_codes(N, [Code]),
	(Code >= 97, Code =< 122),
	(F == h; F == v),
	(L == 2; L == 3),
	(R >= 1, R =< 6),
	(C >= 1, C =< 6).

out_of_edge(h/L/_/C) :- C + L - 1 > 6.
out_of_edge(v/L/R/_) :- R + L - 1 > 6.

overlap(F/L/R/C) :-
	puzzle(P),
	rc_list(F/L/R/C, RCs),
	member(R1/C1, RCs),
	blocked(P, R1/C1, _), !.

exist(N) :- puzzle(P), member(N/_/_/_/_, P).

place_block(N/F/L/R/C) :-
	puzzle(P),
	sort([N/F/L/R/C|P], P1),
	retract(puzzle(_)),
	assert(puzzle(P1)).

print_puzzle(P) :-
	nl,
	member(R, [6,5,4,3,2,1]),
	member(C, [1,2,3,4,5,6]),
	get_square(P, R/C, Square),
	print_square(R/C, Square),
	fail.
print_puzzle(_).

get_square(P, R/C, 46) :- \+ blocked(P, R/C, _), !.
get_square(P, R/C, Code) :- blocked(P, R/C, N), atom_to_capital_letter(N, Code).

print_square(R/1, Square) :- format('~5c~d ~c ', [32,R,Square]), !.
print_square(4/6, Square) :- format('~c =>~n', [Square]), !.
print_square(1/6, Square) :- format('~c~n~7c1 2 3 4 5 6~n', [Square,32]), !.
print_square(_/6, Square) :- format('~c~n', [Square]), !.
print_square(_, Square)   :- format('~c ', [Square]).

print_solution([], _) :- write('  <<< Puzzle Solved >>>'), nl.
print_solution([N/M/R/C|T], TotalSteps) :-
	puzzle(P),
	move(P, P1, N/M/R/C),
	print_puzzle(P1),
	atom_to_capital_letter(N, Block),
	step_number([N/M/R/C|T], TotalSteps, StepNum),
	format('~t~d~5+. [~c] move ~a ', [StepNum,Block,M]),
	wait_for_spacebar, nl,
	retract(puzzle(_)),
	assert(puzzle(P1)),
	print_solution(T, TotalSteps).

atom_to_capital_letter(N, CL) :-
	atom_codes(N, [Code]), CL is Code - 32.

step_number(MoveLeft, TotalSteps, StepNum) :-
	length(MoveLeft, X), StepNum is TotalSteps - X + 1.

wait_for_spacebar :- repeat, get_single_char(Key), Key = 32.
/* --- Generate new states ------------------------------------------------- */
rc_list(h/2/R/C, [R/C, R/C1]) :- C1 is C + 1, !.
rc_list(h/3/R/C, [R/C, R/C1, R/C2]) :- C1 is C + 1, C2 is C + 2, !.
rc_list(v/2/R/C, [R/C, R1/C]) :- R1 is R + 1, !.
rc_list(v/3/R/C, [R/C, R1/C, R2/C]) :- R1 is R + 1, R2 is R + 2.

blocked(State, R/C, N) :-
	member(N/F1/L1/R1/C1, State),
	rc_list(F1/L1/R1/C1, RCs),
	member(R/C, RCs), !.

max_left(State, R/C, 0) :- C1 is C - 1, (C1 < 1; blocked(State, R/C1, _)), !.
max_left(State, R/C, MaxLeft) :- C1 is C - 1, max_left(State, R/C1, X), MaxLeft is X - 1.

max_right(State, L/R/C, 0) :- C1 is C + L, (C1 > 6; blocked(State, R/C1, _)), !.
max_right(State, L/R/C, MaxRight) :- C1 is C + 1, max_right(State, L/R/C1, X), MaxRight is X + 1.

max_up(State, L/R/C, 0) :- R1 is R + L, (R1 > 6; blocked(State, R1/C, _)), !.
max_up(State, L/R/C, MaxUp) :- R1 is R + 1, max_up(State, L/R1/C, X), MaxUp is X + 1.

max_down(State, R/C, 0) :- R1 is R - 1, (R1 < 1; blocked(State, R1/C, _)), !.
max_down(State, R/C, MaxDown) :- R1 is R - 1, max_down(State, R1/C, X), MaxDown is X - 1.

valid_move(State, N/left/R/Cn) :-
	member(N/h/_/R/C, State),
	max_left(State, R/C, MaxLeft),
	MaxLeft \= 0,
	between(MaxLeft, -1, Step),
	Cn is C + Step.
	
valid_move(State, N/right/R/Cn) :-
	member(N/h/L/R/C, State),
	max_right(State, L/R/C, MaxRight),
	MaxRight \= 0,
	between(1, MaxRight, Step),
	Cn is C + Step.

valid_move(State, N/up/Rn/C) :-
	member(N/v/L/R/C, State),
	max_up(State, L/R/C, MaxUp),
	MaxUp \= 0,
	between(1, MaxUp, Step),
	Rn is R + Step.

valid_move(State, N/down/Rn/C) :-
	member(N/v/_/R/C, State),
	max_down(State, R/C, MaxDown),
	MaxDown \= 0,
	between(MaxDown, -1, Step),
	Rn is R + Step.

next_state(State, N/R/C, StateNext) :-
	member(N/F1/L1/R1/C1, State),
	delete(State, N/F1/L1/R1/C1, X),
	sort([N/F1/L1/R/C|X], StateNext).

move(State, StateNext, N/M/R/C) :-
	valid_move(State, N/M/R/C),
	next_state(State, N/R/C, StateNext).
/* --- A* algorithm  ------------------------------------------------------- */
goal(Node) :- member(x/h/2/4/5, Node).

% Reference: prolog :- tutorial by J. R. Fisher
% Web site: https://skolemmachines.org/ThePrologTutorial/www/prolog_tutorial/pt_framer.html

% Node builder notation
:- op(400, yfx, '#').

% Nodes have form S#D#F#A
% 	where S describes the state or configuration
% 	D is the depth of the node
%	F is the evaluation function value
%	A is the ancestor list for the node

solve(Start, Soln) :-
	f_function(Start, 0, F),
	search([Start#0#F#[]], S),
	reverse(S, Soln).

f_function(State, D, F) :-
	h_function(State, H),
	F is D + H.
/*
h_function(State, H) :-
	member(x/h/2/4/C, State),
	H is 5 - C.
*/
h_function(State, H) :-
	findall(yes, barricade(State), X),
	length(X, H).

barricade(State) :-
	member(x/h/2/4/Cx, State),
	Cr is Cx + 2,
	member(_/v/L/R/C, State),
	C >= Cr,
	rc_list(v/L/R/C, RCs),
	member(4/_, RCs).

search([State#_#_#Soln|_], Soln) :- goal(State).
search([B|R], S) :-
	expand(B, Children),
	insert_all(Children, R, Open),
	search(Open, S).

expand(State#D#_#A, All_My_Children) :-
	bagof(Child#D1#F#[Move|A],
			(D1 is D + 1,
			 move(State, Child, Move),
			 f_function(Child, D1, F)),
		  All_My_Children).

insert_all([F|R], Open1, Open3) :-
	insert(F, Open1, Open2),
	insert_all(R, Open2, Open3).
insert_all([], Open, Open).

insert(B, Open ,Open) :- repeat_node(B, Open), !.
insert(B, [C|R], [B,C|R]) :- cheaper(B, C), !.
insert(B, [B1|R], [B1|S]) :- insert(B, R, S), !.
insert(B, [], [B]).

repeat_node(P#_#_#_, [P#_#_#_|_]).
cheaper(_#_#F1#_, _#_#F2#_) :- F1 < F2.

:- unblock_me.
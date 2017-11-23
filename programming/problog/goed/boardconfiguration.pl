:- use_module(library(lists)).

width(3).
height(2).

1/4::uniform_color(red,X,Y);1/4::uniform_color(green,X,Y);1/4::uniform_color(blue,X,Y);1/4::uniform_color(yellow,X,Y).

color(red).
color(green).
color(blue).
color(yellow).

position(X,Y) :-
	width(Width),
	height(Height),
	between(0,Width,X),
	between(0,Height,Y).
	
block(red,X,Y) :- 
	position(X,Y),
	uniform_color(red,X,Y),
	block_conditions(red,X,Y).
	
block(green,X,Y) :- 
	position(X, Y),
	uniform_color(green,X,Y),
	block_conditions(green,X,Y).
	
block(blue,X,Y) :-
	position(X,Y),
	uniform_color(blue,X,Y),
	block_conditions(blue,X,Y).
	
block(yellow,X,Y) :- 
	position(X,Y),
	uniform_color(yellow,X,Y),
	block_conditions(yellow,X,Y).
	
block_conditions(Color,X,Y) :-
	X2 is X + 1,
	\+ block(Color,X2,Y).
block_conditions(Color,X,Y) :-
	X2 is X + 2,
	\+ block(Color,X2,Y).
	
	
initial_board(Board) :-
	create_board(Board).

create_board(Board) :-
	create_board(0,Board,[]).
create_board(Y,Board,BoardAcc) :-
	height(Height),
	Y < Height,
	create_row(Y,Row),
	NewBoardAcc = [Row|BoardAcc],
	NewY is Y + 1,
	create_board(NewY,Board,NewBoardAcc).
create_board(Y,Board,BoardAcc) :-
	height(Y),
	create_row(Y,Row),
	Board = [Row|BoardAcc].
	
create_row(Y,Row) :-
	create_row(0,Y,Row,[]).
create_row(X,Y,Row,RowAcc) :-
	width(Width),
	X < Width,
	block(Color,X,Y),
	append(RowAcc,[block(Color,X,Y)],NewRowAcc),
	NewX is X + 1,
	create_row(NewX,Y,Row,NewRowAcc).
create_row(X,Y,Row,RowAcc) :-
	width(X),
	block(Color,X,Y),
	append(RowAcc,[block(Color,X,Y)],Row).
	
% evidence(block(red,		0,0)).
% evidence(block(blue,	1,0)).
% evidence(block(green,	2,0)).
% evidence(block(red,		0,1)).
% evidence(block(yellow,	1,1)).
% evidence(block(yellow,	2,1)).
% evidence(block(blue,	0,2)).
% evidence(block(red,		1,2)).
% evidence(block(blue,		2,2)).

% query(initial_board(Board)).
query(block(Color,1,2)).
query(block(Color,2,2)).
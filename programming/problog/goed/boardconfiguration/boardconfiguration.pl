:- use_module(library(lists)).

width(2).
height(2).
pressable_color(red).
pressable_color(green).
pressable_color(blue).
pressable_color(yellow).

%%%%%%%%%%%%%%%%%
% RANDOM EVENTS
%%%%%%%%%%%%%%%%%
1/4::uniform_color(red,X,Y);1/4::uniform_color(green,X,Y);1/4::uniform_color(blue,X,Y);1/4::uniform_color(yellow,X,Y).

position(X,Y) :-
	width(Width),
	height(Height),
	between(0,Width,X),
	between(0,Height,Y).
	
block(Color,X,Y) :- 
	position(X,Y),
	uniform_color(Color,X,Y),
	block_constraints(Color,X,Y).

%constraints 3x3
%constraints block20
block_constraints(Color,X,Y) :-
	Xmin1 is X - 1,
	\+ position(Xmin1,Y),
	Xplus1 is X + 1,
	position(Xplus1,Y),
	Xplus2 is X + 2,
	position(Xplus2,Y),
	Yplus1 is Y + 1,
	\+ position(X,Yplus1),
	Ymin1 is Y - 1,
	position(X,Ymin1),
	Ymin2 is Y - 2,
	position(X,Ymin2),
	pressable_color(Color2),
	Color2 \= Color,
	uniform_color(Color2,Xplus1,Y),
	pressable_color(Color3),
	Color3 \= Color,
	uniform_color(Color3,Xplus2,Y).
block_constraints(Color,X,Y) :-
	Xmin1 is X - 1,
	\+ position(Xmin1,Y),
	Xplus1 is X + 1,
	position(Xplus1,Y),
	Xplus2 is X + 2,
	position(Xplus2,Y),
	Yplus1 is Y + 1,
	\+ position(X,Yplus1),
	Ymin1 is Y - 1,
	position(X,Ymin1),
	Ymin2 is Y - 2,
	position(X,Ymin2),
	uniform_color(Color,Xplus1,Y),
	pressable_color(Color2),
	Color2 \= Color,
	uniform_color(Color2,Xplus2,Y).
block_constraints(Color,X,Y) :-
	Xmin1 is X - 1,
	\+ position(Xmin1,Y),
	Xplus1 is X + 1,
	position(Xplus1,Y),
	Xplus2 is X + 2,
	position(Xplus2,Y),
	Yplus1 is Y + 1,
	\+ position(X,Yplus1),
	Ymin1 is Y - 1,
	position(X,Ymin1),
	Ymin2 is Y - 2,
	position(X,Ymin2),
	pressable_color(Color2),
	Color2 \= Color,
	uniform_color(Color2,Xplus1,Y),
	uniform_color(Color,Xplus2,Y).
	
	

	
%%%%%%%%%%%%%%%%%
% GAME PREDICATES
%%%%%%%%%%%%%%%%%
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
	
evidence(block(blue,0,2)). evidence(block(red,1,2)). evidence(block(blue,2,2)).
evidence(block(red,0,1)). evidence(block(yellow,1,1)). evidence(block(yellow,2,1)).
evidence(block(red,0,0)). evidence(block(blue,1,0)). evidence(block(green,2,0)).

query(initial_board(Board)).
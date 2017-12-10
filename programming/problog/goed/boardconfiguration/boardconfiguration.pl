:- use_module(library(lists)).

width(4).
height(4).
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
	height(Y),
	X = 0,
	uniform_color(Color,X,Y),
	block_top_left_constraints(Color,X,Y).
block(Color,X,Y) :-
	height(Y),
	X = 1,
	uniform_color(Color,X,Y),
	block_top_second_left_constraints(Color,X,Y).
block(Color,X,Y) :-
	height(Y),
	width(Width),
	MaxX is Width - 1,
	X > 1,
	X < MaxX,
	uniform_color(Color,X,Y),
	block_top_middle_constraints(Color,X,Y).
block(Color,X,Y) :-
	height(Y),
	width(Width),
	XCheck is Width - 1,
	X = XCheck,
	uniform_color(Color,X,Y),
	block_top_second_right_constraints(Color,X,Y).
block(Color,X,Y) :-
	height(Y),
	width(X),
	uniform_color(Color,X,Y),
	block_top_right_constraints(Color,X,Y).


%constraints 5x5
block_top_left_constraints(Color,X,Y) :-
	x_min_1_no_position(X,Y),
	x_plus_1_position(X,Y,Xplus1),
	x_plus_2_position(X,Y,Xplus2),
	y_plus_1_no_position(X,Y),
	y_min_1_position(X,Y,Ymin1),
	y_min_2_position(X,Y,Ymin2),
	block04_constaints(Color,X,Y,Xplus1,Xplus2,Ymin1,Ymin2).
block_top_second_left_constraints(Color,X,Y) :-
	x_min_1_position(X,Y,Xmin1),
	x_min_2_no_position(X,Y),
	x_plus_1_position(X,Y,Xplus1),
	x_plus_2_position(X,Y,Xplus2),
	y_plus_1_no_position(X,Y),
	y_min_1_position(X,Y,Ymin1),
	y_min_2_position(X,Y,Ymin2),
	block14_constaints(Color,X,Y,Xmin1,Xplus1,Xplus2,Ymin1,Ymin2).
block_top_middle_constraints(Color,X,Y) :-
	x_min_1_position(X,Y,Xmin1),
	x_min_2_position(X,Y,Xmin2),
	x_plus_1_position(X,Y,Xplus1),
	x_plus_2_position(X,Y,Xplus2),
	y_plus_1_no_position(X,Y),
	y_min_1_position(X,Y,Ymin1),
	y_min_2_position(X,Y,Ymin2),
	block24_constaints(Color,X,Y,Xmin1,Xmin2,Xplus1,Xplus2,Ymin1,Ymin2).
block_top_second_right_constraints(Color,X,Y) :-
	x_min_1_position(X,Y,Xmin1),
	x_min_2_position(X,Y,Xmin2),
	x_plus_1_position(X,Y,Xplus1),
	x_plus_2_no_position(X,Y),
	y_plus_1_no_position(X,Y),
	y_min_1_position(X,Y,Ymin1),
	y_min_2_position(X,Y,Ymin2),
	block34_constaints(Color,X,Y,Xmin1,Xmin2,Xplus1,Ymin1,Ymin2).
block_top_right_constraints(Color,X,Y) :-
	x_min_1_position(X,Y,Xmin1),
	x_min_2_position(X,Y,Xmin2),
	x_plus_1_no_position(X,Y),
	y_plus_1_no_position(X,Y),
	y_min_1_position(X,Y,Ymin1),
	y_min_2_position(X,Y,Ymin2),
	block44_constaints(Color,X,Y,Xmin1,Xmin2,Ymin1,Ymin2).
	
	
% constraints block04
block04_constaints(Color,X,Y,Xplus1,Xplus2,Ymin1,Ymin2) :-
	X = 0,
	\+ uniform_color(Color,Xplus1,Y),
	\+ uniform_color(Color,Xplus2,Y),
	\+ uniform_color(Color,Ymin1,Y),
	\+ uniform_color(Color,Ymin2,Y).
block04_constaints(Color,X,Y,Xplus1,Xplus2,Ymin1,Ymin2) :- X = 0,
	\+ uniform_color(Color,Xplus1,Y),
	\+ uniform_color(Color,Xplus2,Y),
	\+ uniform_color(Color,Ymin1,Y),
	uniform_color(Color,Ymin2,Y).
block04_constaints(Color,X,Y,Xplus1,Xplus2,Ymin1,Ymin2) :- X = 0,
	\+ uniform_color(Color,Xplus1,Y),
	\+ uniform_color(Color,Xplus2,Y),
	uniform_color(Color,Ymin1,Y),
	\+ uniform_color(Color,Ymin2,Y).
block04_constaints(Color,X,Y,Xplus1,Xplus2,Ymin1,Ymin2) :- X = 0,
	\+ uniform_color(Color,Xplus1,Y),
	uniform_color(Color,Xplus2,Y),
	\+ uniform_color(Color,Ymin1,Y),
	\+ uniform_color(Color,Ymin2,Y).
block04_constaints(Color,X,Y,Xplus1,Xplus2,Ymin1,Ymin2) :- X = 0,
	\+ uniform_color(Color,Xplus1,Y),
	uniform_color(Color,Xplus2,Y),
	\+ uniform_color(Color,Ymin1,Y),
	uniform_color(Color,Ymin2,Y).
block04_constaints(Color,X,Y,Xplus1,Xplus2,Ymin1,Ymin2) :- X = 0,
	\+ uniform_color(Color,Xplus1,Y),
	uniform_color(Color,Xplus2,Y),
	uniform_color(Color,Ymin1,Y),
	\+ uniform_color(Color,Ymin2,Y).
block04_constaints(Color,X,Y,Xplus1,Xplus2,Ymin1,Ymin2) :- X = 0,
	uniform_color(Color,Xplus1,Y),
	\+ uniform_color(Color,Xplus2,Y),
	\+ uniform_color(Color,Ymin1,Y),
	\+ uniform_color(Color,Ymin2,Y).
block04_constaints(Color,X,Y,Xplus1,Xplus2,Ymin1,Ymin2) :- X = 0,
	uniform_color(Color,Xplus1,Y),
	\+ uniform_color(Color,Xplus2,Y),
	\+ uniform_color(Color,Ymin1,Y),
	uniform_color(Color,Ymin2,Y).
block04_constaints(Color,X,Y,Xplus1,Xplus2,Ymin1,Ymin2) :- X = 0,
	uniform_color(Color,Xplus1,Y),
	\+ uniform_color(Color,Xplus2,Y),
	uniform_color(Color,Ymin1,Y),
	\+ uniform_color(Color,Ymin2,Y).


% constraints block14
block14_constaints(Color,X,Y,Xmin1,Xplus1,Xplus2,Ymin1,Ymin2) :- X = 1,
	\+ uniform_color(Color,Xmin1,Y),
	\+ uniform_color(Color,Xplus1,Y),
	\+ uniform_color(Color,Xplus2,Y),
	\+ uniform_color(Color,Ymin1,Y),
	\+ uniform_color(Color,Ymin2,Y).
block14_constaints(Color,X,Y,Xmin1,Xplus1,Xplus2,Ymin1,Ymin2) :- X = 1,
	\+ uniform_color(Color,Xmin1,Y),
	\+ uniform_color(Color,Xplus1,Y),
	\+ uniform_color(Color,Xplus2,Y),
	\+ uniform_color(Color,Ymin1,Y),
	uniform_color(Color,Ymin2,Y).
block14_constaints(Color,X,Y,Xmin1,Xplus1,Xplus2,Ymin1,Ymin2) :- X = 1,
	\+ uniform_color(Color,Xmin1,Y),
	\+ uniform_color(Color,Xplus1,Y),
	\+ uniform_color(Color,Xplus2,Y),
	uniform_color(Color,Ymin1,Y),
	\+ uniform_color(Color,Ymin2,Y).
block14_constaints(Color,X,Y,Xmin1,Xplus1,Xplus2,Ymin1,Ymin2) :- X = 1,
	\+ uniform_color(Color,Xmin1,Y),
	\+ uniform_color(Color,Xplus1,Y),
	uniform_color(Color,Xplus2,Y),
	\+ uniform_color(Color,Ymin1,Y),
	\+ uniform_color(Color,Ymin2,Y).
block14_constaints(Color,X,Y,Xmin1,Xplus1,Xplus2,Ymin1,Ymin2) :- X = 1,
	\+ uniform_color(Color,Xmin1,Y),
	\+ uniform_color(Color,Xplus1,Y),
	uniform_color(Color,Xplus2,Y),
	\+ uniform_color(Color,Ymin1,Y),
	uniform_color(Color,Ymin2,Y).
block14_constaints(Color,X,Y,Xmin1,Xplus1,Xplus2,Ymin1,Ymin2) :- X = 1,
	\+ uniform_color(Color,Xmin1,Y),
	\+ uniform_color(Color,Xplus1,Y),
	uniform_color(Color,Xplus2,Y),
	uniform_color(Color,Ymin1,Y),
	\+ uniform_color(Color,Ymin2,Y).
block14_constaints(Color,X,Y,Xmin1,Xplus1,Xplus2,Ymin1,Ymin2) :- X = 1,
	\+ uniform_color(Color,Xmin1,Y),
	uniform_color(Color,Xplus1,Y),
	\+ uniform_color(Color,Xplus2,Y),
	\+ uniform_color(Color,Ymin1,Y),
	\+ uniform_color(Color,Ymin2,Y).
block14_constaints(Color,X,Y,Xmin1,Xplus1,Xplus2,Ymin1,Ymin2) :- X = 1,
	\+ uniform_color(Color,Xmin1,Y),
	uniform_color(Color,Xplus1,Y),
	\+ uniform_color(Color,Xplus2,Y),
	\+ uniform_color(Color,Ymin1,Y),
	uniform_color(Color,Ymin2,Y).
block14_constaints(Color,X,Y,Xmin1,Xplus1,Xplus2,Ymin1,Ymin2) :- X = 1,
	\+ uniform_color(Color,Xmin1,Y),
	uniform_color(Color,Xplus1,Y),
	\+ uniform_color(Color,Xplus2,Y),
	uniform_color(Color,Ymin1,Y),
	\+ uniform_color(Color,Ymin2,Y).
block14_constaints(Color,X,Y,Xmin1,Xplus1,Xplus2,Ymin1,Ymin2) :- X = 1,
	uniform_color(Color,Xmin1,Y),
	\+ uniform_color(Color,Xplus1,Y),
	\+ uniform_color(Color,Xplus2,Y),
	\+ uniform_color(Color,Ymin1,Y),
	\+ uniform_color(Color,Ymin2,Y).
block14_constaints(Color,X,Y,Xmin1,Xplus1,Xplus2,Ymin1,Ymin2) :- X = 1,
	uniform_color(Color,Xmin1,Y),
	\+ uniform_color(Color,Xplus1,Y),
	\+ uniform_color(Color,Xplus2,Y),
	\+ uniform_color(Color,Ymin1,Y),
	uniform_color(Color,Ymin2,Y).
block14_constaints(Color,X,Y,Xmin1,Xplus1,Xplus2,Ymin1,Ymin2) :- X = 1,
	uniform_color(Color,Xmin1,Y),
	\+ uniform_color(Color,Xplus1,Y),
	\+ uniform_color(Color,Xplus2,Y),
	uniform_color(Color,Ymin1,Y),
	\+ uniform_color(Color,Ymin2,Y).
block14_constaints(Color,X,Y,Xmin1,Xplus1,Xplus2,Ymin1,Ymin2) :- X = 1,
	uniform_color(Color,Xmin1,Y),
	\+ uniform_color(Color,Xplus1,Y),
	uniform_color(Color,Xplus2,Y),
	\+ uniform_color(Color,Ymin1,Y),
	\+ uniform_color(Color,Ymin2,Y).
block14_constaints(Color,X,Y,Xmin1,Xplus1,Xplus2,Ymin1,Ymin2) :- X = 1,
	uniform_color(Color,Xmin1,Y),
	\+ uniform_color(Color,Xplus1,Y),
	uniform_color(Color,Xplus2,Y),
	\+ uniform_color(Color,Ymin1,Y),
	uniform_color(Color,Ymin2,Y).
block14_constaints(Color,X,Y,Xmin1,Xplus1,Xplus2,Ymin1,Ymin2) :- X = 1,
	uniform_color(Color,Xmin1,Y),
	\+ uniform_color(Color,Xplus1,Y),
	uniform_color(Color,Xplus2,Y),
	uniform_color(Color,Ymin1,Y),
	\+ uniform_color(Color,Ymin2,Y).
	
% constraints block24
block24_constaints(Color,X,Y,Xmin1,Xmin2,Xplus1,Xplus2,Ymin1,Ymin2) :- X = 2,
	\+ uniform_color(Color,Xmin2,Y),
	\+ uniform_color(Color,Xmin1,Y),
	\+ uniform_color(Color,Xplus1,Y),
	\+ uniform_color(Color,Xplus2,Y),
	\+ uniform_color(Color,Ymin1,Y),
	\+ uniform_color(Color,Ymin2,Y).
block24_constaints(Color,X,Y,Xmin1,Xmin2,Xplus1,Xplus2,Ymin1,Ymin2) :- X = 2,
	\+ uniform_color(Color,Xmin2,Y),
	\+ uniform_color(Color,Xmin1,Y),
	\+ uniform_color(Color,Xplus1,Y),
	\+ uniform_color(Color,Xplus2,Y),
	\+ uniform_color(Color,Ymin1,Y),
	uniform_color(Color,Ymin2,Y).
block24_constaints(Color,X,Y,Xmin1,Xmin2,Xplus1,Xplus2,Ymin1,Ymin2) :- X = 2,
	\+ uniform_color(Color,Xmin2,Y),
	\+ uniform_color(Color,Xmin1,Y),
	\+ uniform_color(Color,Xplus1,Y),
	\+ uniform_color(Color,Xplus2,Y),
	uniform_color(Color,Ymin1,Y),
	\+ uniform_color(Color,Ymin2,Y).
block24_constaints(Color,X,Y,Xmin1,Xmin2,Xplus1,Xplus2,Ymin1,Ymin2) :- X = 2,
	\+ uniform_color(Color,Xmin2,Y),
	\+ uniform_color(Color,Xmin1,Y),
	\+ uniform_color(Color,Xplus1,Y),
	uniform_color(Color,Xplus2,Y),
	\+ uniform_color(Color,Ymin1,Y),
	\+ uniform_color(Color,Ymin2,Y).
block24_constaints(Color,X,Y,Xmin1,Xmin2,Xplus1,Xplus2,Ymin1,Ymin2) :- X = 2,
	\+ uniform_color(Color,Xmin2,Y),
	\+ uniform_color(Color,Xmin1,Y),
	\+ uniform_color(Color,Xplus1,Y),
	uniform_color(Color,Xplus2,Y),
	\+ uniform_color(Color,Ymin1,Y),
	uniform_color(Color,Ymin2,Y).
block24_constaints(Color,X,Y,Xmin1,Xmin2,Xplus1,Xplus2,Ymin1,Ymin2) :- X = 2,
	\+ uniform_color(Color,Xmin2,Y),
	\+ uniform_color(Color,Xmin1,Y),
	\+ uniform_color(Color,Xplus1,Y),
	uniform_color(Color,Xplus2,Y),
	uniform_color(Color,Ymin1,Y),
	\+ uniform_color(Color,Ymin2,Y).
block24_constaints(Color,X,Y,Xmin1,Xmin2,Xplus1,Xplus2,Ymin1,Ymin2) :- X = 2,
	\+ uniform_color(Color,Xmin2,Y),
	\+ uniform_color(Color,Xmin1,Y),
	uniform_color(Color,Xplus1,Y),
	\+ uniform_color(Color,Xplus2,Y),
	\+ uniform_color(Color,Ymin1,Y),
	\+ uniform_color(Color,Ymin2,Y).
block24_constaints(Color,X,Y,Xmin1,Xmin2,Xplus1,Xplus2,Ymin1,Ymin2) :- X = 2,
	\+ uniform_color(Color,Xmin2,Y),
	\+ uniform_color(Color,Xmin1,Y),
	uniform_color(Color,Xplus1,Y),
	\+ uniform_color(Color,Xplus2,Y),
	\+ uniform_color(Color,Ymin1,Y),
	uniform_color(Color,Ymin2,Y).
block24_constaints(Color,X,Y,Xmin1,Xmin2,Xplus1,Xplus2,Ymin1,Ymin2) :- X = 2,
	\+ uniform_color(Color,Xmin2,Y),
	\+ uniform_color(Color,Xmin1,Y),
	uniform_color(Color,Xplus1,Y),
	\+ uniform_color(Color,Xplus2,Y),
	uniform_color(Color,Ymin1,Y),
	\+ uniform_color(Color,Ymin2,Y).
block24_constaints(Color,X,Y,Xmin1,Xmin2,Xplus1,Xplus2,Ymin1,Ymin2) :- X = 2,
	\+ uniform_color(Color,Xmin2,Y),
	uniform_color(Color,Xmin1,Y),
	\+ uniform_color(Color,Xplus1,Y),
	\+ uniform_color(Color,Xplus2,Y),
	\+ uniform_color(Color,Ymin1,Y),
	\+ uniform_color(Color,Ymin2,Y).
block24_constaints(Color,X,Y,Xmin1,Xmin2,Xplus1,Xplus2,Ymin1,Ymin2) :- X = 2,
	\+ uniform_color(Color,Xmin2,Y),
	uniform_color(Color,Xmin1,Y),
	\+ uniform_color(Color,Xplus1,Y),
	\+ uniform_color(Color,Xplus2,Y),
	\+ uniform_color(Color,Ymin1,Y),
	uniform_color(Color,Ymin2,Y).
block24_constaints(Color,X,Y,Xmin1,Xmin2,Xplus1,Xplus2,Ymin1,Ymin2) :- X = 2,
	\+ uniform_color(Color,Xmin2,Y),
	uniform_color(Color,Xmin1,Y),
	\+ uniform_color(Color,Xplus1,Y),
	\+ uniform_color(Color,Xplus2,Y),
	uniform_color(Color,Ymin1,Y),
	\+ uniform_color(Color,Ymin2,Y).
block24_constaints(Color,X,Y,Xmin1,Xmin2,Xplus1,Xplus2,Ymin1,Ymin2) :- X = 2,
	\+ uniform_color(Color,Xmin2,Y),
	uniform_color(Color,Xmin1,Y),
	\+ uniform_color(Color,Xplus1,Y),
	uniform_color(Color,Xplus2,Y),
	\+ uniform_color(Color,Ymin1,Y),
	\+ uniform_color(Color,Ymin2,Y).
block24_constaints(Color,X,Y,Xmin1,Xmin2,Xplus1,Xplus2,Ymin1,Ymin2) :- X = 2,
	\+ uniform_color(Color,Xmin2,Y),
	uniform_color(Color,Xmin1,Y),
	\+ uniform_color(Color,Xplus1,Y),
	uniform_color(Color,Xplus2,Y),
	\+ uniform_color(Color,Ymin1,Y),
	uniform_color(Color,Ymin2,Y).
block24_constaints(Color,X,Y,Xmin1,Xmin2,Xplus1,Xplus2,Ymin1,Ymin2) :- X = 2,
	\+ uniform_color(Color,Xmin2,Y),
	uniform_color(Color,Xmin1,Y),
	\+ uniform_color(Color,Xplus1,Y),
	uniform_color(Color,Xplus2,Y),
	uniform_color(Color,Ymin1,Y),
	\+ uniform_color(Color,Ymin2,Y).
block24_constaints(Color,X,Y,Xmin1,Xmin2,Xplus1,Xplus2,Ymin1,Ymin2) :- X = 2,
	uniform_color(Color,Xmin2,Y),
	\+ uniform_color(Color,Xmin1,Y),
	\+ uniform_color(Color,Xplus1,Y),
	\+ uniform_color(Color,Xplus2,Y),
	\+ uniform_color(Color,Ymin1,Y),
	\+ uniform_color(Color,Ymin2,Y).
block24_constaints(Color,X,Y,Xmin1,Xmin2,Xplus1,Xplus2,Ymin1,Ymin2) :- X = 2,
	uniform_color(Color,Xmin2,Y),
	\+ uniform_color(Color,Xmin1,Y),
	\+ uniform_color(Color,Xplus1,Y),
	\+ uniform_color(Color,Xplus2,Y),
	\+ uniform_color(Color,Ymin1,Y),
	uniform_color(Color,Ymin2,Y).
block24_constaints(Color,X,Y,Xmin1,Xmin2,Xplus1,Xplus2,Ymin1,Ymin2) :- X = 2,
	uniform_color(Color,Xmin2,Y),
	\+ uniform_color(Color,Xmin1,Y),
	\+ uniform_color(Color,Xplus1,Y),
	\+ uniform_color(Color,Xplus2,Y),
	uniform_color(Color,Ymin1,Y),
	\+ uniform_color(Color,Ymin2,Y).
block24_constaints(Color,X,Y,Xmin1,Xmin2,Xplus1,Xplus2,Ymin1,Ymin2) :- X = 2,
	uniform_color(Color,Xmin2,Y),
	\+ uniform_color(Color,Xmin1,Y),
	\+ uniform_color(Color,Xplus1,Y),
	uniform_color(Color,Xplus2,Y),
	\+ uniform_color(Color,Ymin1,Y),
	\+ uniform_color(Color,Ymin2,Y).
block24_constaints(Color,X,Y,Xmin1,Xmin2,Xplus1,Xplus2,Ymin1,Ymin2) :- X = 2,
	uniform_color(Color,Xmin2,Y),
	\+ uniform_color(Color,Xmin1,Y),
	\+ uniform_color(Color,Xplus1,Y),
	uniform_color(Color,Xplus2,Y),
	\+ uniform_color(Color,Ymin1,Y),
	uniform_color(Color,Ymin2,Y).
block24_constaints(Color,X,Y,Xmin1,Xmin2,Xplus1,Xplus2,Ymin1,Ymin2) :- X = 2,
	uniform_color(Color,Xmin2,Y),
	\+ uniform_color(Color,Xmin1,Y),
	\+ uniform_color(Color,Xplus1,Y),
	uniform_color(Color,Xplus2,Y),
	uniform_color(Color,Ymin1,Y),
	\+ uniform_color(Color,Ymin2,Y).
block24_constaints(Color,X,Y,Xmin1,Xmin2,Xplus1,Xplus2,Ymin1,Ymin2) :- X = 2,
	uniform_color(Color,Xmin2,Y),
	\+ uniform_color(Color,Xmin1,Y),
	uniform_color(Color,Xplus1,Y),
	\+ uniform_color(Color,Xplus2,Y),
	\+ uniform_color(Color,Ymin1,Y),
	\+ uniform_color(Color,Ymin2,Y).
block24_constaints(Color,X,Y,Xmin1,Xmin2,Xplus1,Xplus2,Ymin1,Ymin2) :- X = 2,
	uniform_color(Color,Xmin2,Y),
	\+ uniform_color(Color,Xmin1,Y),
	uniform_color(Color,Xplus1,Y),
	\+ uniform_color(Color,Xplus2,Y),
	\+ uniform_color(Color,Ymin1,Y),
	uniform_color(Color,Ymin2,Y).
block24_constaints(Color,X,Y,Xmin1,Xmin2,Xplus1,Xplus2,Ymin1,Ymin2) :- X = 2,
	uniform_color(Color,Xmin2,Y),
	\+ uniform_color(Color,Xmin1,Y),
	uniform_color(Color,Xplus1,Y),
	\+ uniform_color(Color,Xplus2,Y),
	uniform_color(Color,Ymin1,Y),
	\+ uniform_color(Color,Ymin2,Y).
	
% constraints block34
block34_constaints(Color,X,Y,Xmin1,Xmin2,Xplus1,Ymin1,Ymin2) :- X = 3,
	\+ uniform_color(Color,Xmin2,Y),
	\+ uniform_color(Color,Xmin1,Y),
	\+ uniform_color(Color,Xplus1,Y),
	\+ uniform_color(Color,Ymin1,Y),
	\+ uniform_color(Color,Ymin2,Y).
block34_constaints(Color,X,Y,Xmin1,Xmin2,Xplus1,Ymin1,Ymin2) :- X = 3,
	\+ uniform_color(Color,Xmin2,Y),
	\+ uniform_color(Color,Xmin1,Y),
	\+ uniform_color(Color,Xplus1,Y),
	\+ uniform_color(Color,Ymin1,Y),
	uniform_color(Color,Ymin2,Y).
block34_constaints(Color,X,Y,Xmin1,Xmin2,Xplus1,Ymin1,Ymin2) :- X = 3,
	\+ uniform_color(Color,Xmin2,Y),
	\+ uniform_color(Color,Xmin1,Y),
	\+ uniform_color(Color,Xplus1,Y),
	uniform_color(Color,Ymin1,Y),
	\+ uniform_color(Color,Ymin2,Y).
block34_constaints(Color,X,Y,Xmin1,Xmin2,Xplus1,Ymin1,Ymin2) :- X = 3,
	\+ uniform_color(Color,Xmin2,Y),
	\+ uniform_color(Color,Xmin1,Y),
	uniform_color(Color,Xplus1,Y),
	\+ uniform_color(Color,Ymin1,Y),
	\+ uniform_color(Color,Ymin2,Y).
block34_constaints(Color,X,Y,Xmin1,Xmin2,Xplus1,Ymin1,Ymin2) :- X = 3,
	\+ uniform_color(Color,Xmin2,Y),
	\+ uniform_color(Color,Xmin1,Y),
	uniform_color(Color,Xplus1,Y),
	\+ uniform_color(Color,Ymin1,Y),
	uniform_color(Color,Ymin2,Y).	
block34_constaints(Color,X,Y,Xmin1,Xmin2,Xplus1,Ymin1,Ymin2) :- X = 3,
	\+ uniform_color(Color,Xmin2,Y),
	\+ uniform_color(Color,Xmin1,Y),
	uniform_color(Color,Xplus1,Y),
	uniform_color(Color,Ymin1,Y),
	\+ uniform_color(Color,Ymin2,Y).	
block34_constaints(Color,X,Y,Xmin1,Xmin2,Xplus1,Ymin1,Ymin2) :- X = 3,
	\+ uniform_color(Color,Xmin2,Y),
	uniform_color(Color,Xmin1,Y),
	\+ uniform_color(Color,Xplus1,Y),
	\+ uniform_color(Color,Ymin1,Y),
	\+ uniform_color(Color,Ymin2,Y).	
block34_constaints(Color,X,Y,Xmin1,Xmin2,Xplus1,Ymin1,Ymin2) :- X = 3,
	\+ uniform_color(Color,Xmin2,Y),
	uniform_color(Color,Xmin1,Y),
	\+ uniform_color(Color,Xplus1,Y),
	\+ uniform_color(Color,Ymin1,Y),
	uniform_color(Color,Ymin2,Y).
block34_constaints(Color,X,Y,Xmin1,Xmin2,Xplus1,Ymin1,Ymin2) :- X = 3,
	\+ uniform_color(Color,Xmin2,Y),
	uniform_color(Color,Xmin1,Y),
	\+ uniform_color(Color,Xplus1,Y),
	uniform_color(Color,Ymin1,Y),
	\+ uniform_color(Color,Ymin2,Y).
block34_constaints(Color,X,Y,Xmin1,Xmin2,Xplus1,Ymin1,Ymin2) :- X = 3,
	uniform_color(Color,Xmin2,Y),
	\+ uniform_color(Color,Xmin1,Y),
	\+ uniform_color(Color,Xplus1,Y),
	\+ uniform_color(Color,Ymin1,Y),
	\+ uniform_color(Color,Ymin2,Y).
block34_constaints(Color,X,Y,Xmin1,Xmin2,Xplus1,Ymin1,Ymin2) :- X = 3,
	uniform_color(Color,Xmin2,Y),
	\+ uniform_color(Color,Xmin1,Y),
	\+ uniform_color(Color,Xplus1,Y),
	\+ uniform_color(Color,Ymin1,Y),
	uniform_color(Color,Ymin2,Y).
block34_constaints(Color,X,Y,Xmin1,Xmin2,Xplus1,Ymin1,Ymin2) :- X = 3,
	uniform_color(Color,Xmin2,Y),
	\+ uniform_color(Color,Xmin1,Y),
	\+ uniform_color(Color,Xplus1,Y),
	uniform_color(Color,Ymin1,Y),
	\+ uniform_color(Color,Ymin2,Y).
block34_constaints(Color,X,Y,Xmin1,Xmin2,Xplus1,Ymin1,Ymin2) :- X = 3,
	uniform_color(Color,Xmin2,Y),
	\+ uniform_color(Color,Xmin1,Y),
	uniform_color(Color,Xplus1,Y),
	\+ uniform_color(Color,Ymin1,Y),
	\+ uniform_color(Color,Ymin2,Y).
block34_constaints(Color,X,Y,Xmin1,Xmin2,Xplus1,Ymin1,Ymin2) :- X = 3,
	uniform_color(Color,Xmin2,Y),
	\+ uniform_color(Color,Xmin1,Y),
	uniform_color(Color,Xplus1,Y),
	\+ uniform_color(Color,Ymin1,Y),
	uniform_color(Color,Ymin2,Y).
block34_constaints(Color,X,Y,Xmin1,Xmin2,Xplus1,Ymin1,Ymin2) :- X = 3,
	uniform_color(Color,Xmin2,Y),
	\+ uniform_color(Color,Xmin1,Y),
	uniform_color(Color,Xplus1,Y),
	uniform_color(Color,Ymin1,Y),
	\+ uniform_color(Color,Ymin2,Y).
	
% constraints block44
block44_constaints(Color,X,Y,Xmin1,Xmin2,Ymin1,Ymin2) :- X = 4,
	\+ uniform_color(Color,Xmin1,Y),
	\+ uniform_color(Color,Xmin2,Y),
	\+ uniform_color(Color,Ymin1,Y),
	\+ uniform_color(Color,Ymin2,Y).
block44_constaints(Color,X,Y,Xmin1,Xmin2,Ymin1,Ymin2) :- X = 4,
	\+ uniform_color(Color,Xmin1,Y),
	\+ uniform_color(Color,Xmin2,Y),
	\+ uniform_color(Color,Ymin1,Y),
	uniform_color(Color,Ymin2,Y).
block44_constaints(Color,X,Y,Xmin1,Xmin2,Ymin1,Ymin2) :- X = 4,
	\+ uniform_color(Color,Xmin1,Y),
	\+ uniform_color(Color,Xmin2,Y),
	uniform_color(Color,Ymin1,Y),
	\+ uniform_color(Color,Ymin2,Y).
block44_constaints(Color,X,Y,Xmin1,Xmin2,Ymin1,Ymin2) :- X = 4,
	\+ uniform_color(Color,Xmin1,Y),
	uniform_color(Color,Xmin2,Y),
	\+ uniform_color(Color,Ymin1,Y),
	\+ uniform_color(Color,Ymin2,Y).
block44_constaints(Color,X,Y,Xmin1,Xmin2,Ymin1,Ymin2) :- X = 4,
	\+ uniform_color(Color,Xmin1,Y),
	uniform_color(Color,Xmin2,Y),
	\+ uniform_color(Color,Ymin1,Y),
	uniform_color(Color,Ymin2,Y).
block44_constaints(Color,X,Y,Xmin1,Xmin2,Ymin1,Ymin2) :- X = 4,
	\+ uniform_color(Color,Xmin1,Y),
	uniform_color(Color,Xmin2,Y),
	uniform_color(Color,Ymin1,Y),
	\+ uniform_color(Color,Ymin2,Y).
block44_constaints(Color,X,Y,Xmin1,Xmin2,Ymin1,Ymin2) :- X = 4,
	uniform_color(Color,Xmin1,Y),
	\+ uniform_color(Color,Xmin2,Y),
	\+ uniform_color(Color,Ymin1,Y),
	\+ uniform_color(Color,Ymin2,Y).
block44_constaints(Color,X,Y,Xmin1,Xmin2,Ymin1,Ymin2) :- X = 4,
	uniform_color(Color,Xmin1,Y),
	\+ uniform_color(Color,Xmin2,Y),
	\+ uniform_color(Color,Ymin1,Y),
	uniform_color(Color,Ymin2,Y).
block44_constaints(Color,X,Y,Xmin1,Xmin2,Ymin1,Ymin2) :- X = 4,
	uniform_color(Color,Xmin1,Y),
	\+ uniform_color(Color,Xmin2,Y),
	uniform_color(Color,Ymin1,Y),
	\+ uniform_color(Color,Ymin2,Y).

	

x_min_1_no_position(X,Y) :-
	X2 is X - 1,
	\+ position(X2,Y).
x_min_2_no_position(X,Y) :-
	X2 is X - 2,
	\+ position(X2,Y).
x_plus_1_no_position(X,Y) :-
	X2 is X + 1,
	\+ position(X2,Y).
x_plus_2_no_position(X,Y) :-
	X2 is X + 2,
	\+ position(X2,Y).
y_min_1_no_position(X,Y) :-
	Y2 is Y - 1,
	\+ position(X,Y2).
y_min_2_no_position(X,Y) :-
	Y2 is Y - 2,
	\+ position(X,Y2).
y_plus_1_no_position(X,Y) :-
	Y2 is Y + 1,
	\+ position(X,Y2).
y_plus_2_no_position(X,Y) :-
	Y2 is Y + 2,
	\+ position(X,Y2).
x_min_1_position(X,Y,X2) :-
	X2 is X - 1,
	position(X2,Y).
x_min_2_position(X,Y,X2) :-
	X2 is X - 2,
	position(X2,Y).
x_plus_1_position(X,Y,X2) :-
	X2 is X + 1,
	position(X2,Y).
x_plus_2_position(X,Y,X2) :-
	X2 is X + 2,
	position(X2,Y).
y_min_1_position(X,Y,Y2) :-
	Y2 is Y - 1,
	position(X,Y2).
y_min_2_position(X,Y,Y2) :-
	Y2 is Y - 2,
	position(X,Y2).
y_plus_1_position(X,Y,Y2) :-
	Y2 is Y + 1,
	position(X,Y2).
y_plus_2_position(X,Y,Y2) :-
	Y2 is Y + 2,
	position(X,Y2).


	
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
	
row(R) :-
	block(Color1,0,4),
	block(Color2,1,4),
	block(Color3,2,4),
	block(Color4,3,4),
	block(Color5,4,4),
	R = [Color1,Color2,Color3,Color4,Color5].

% evidence(block(red,0,4)). 	
evidence(block(red,1,4)). 		evidence(block(blue,2,4)). 		evidence(block(yellow,3,4)). 	evidence(block(blue,4,4)). 
% evidence(block(red,0,3)). 	evidence(block(yellow,1,3)). 	evidence(block(yellow,2,3)). 	evidence(block(blue,3,3)). 	evidence(block(blue,4,3)). 	
% evidence(block(red,0,2)). 	evidence(block(red,1,2)). 		evidence(block(blue,2,2)). 		evidence(block(blue,3,2)). 	evidence(block(blue,4,2)). 
% evidence(block(red,0,1)). 	evidence(block(yellow,1,1)). 	evidence(block(yellow,2,1)). 	evidence(block(blue,3,1)). 	evidence(block(blue,4,1)). 
% evidence(block(red,0,0)). 	evidence(block(blue,1,0)). 		evidence(block(green,2,0)). 	evidence(block(blue,3,0)). 	evidence(block(blue,4,0)). 

% query(initial_board(Board)).
% query(create_row(4,Row)).
query(row(R)).
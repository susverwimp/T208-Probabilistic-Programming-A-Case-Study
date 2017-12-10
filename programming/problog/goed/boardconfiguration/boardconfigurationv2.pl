:- use_module(library(lists)).

pressable_color(red).
pressable_color(green).
pressable_color(blue).
pressable_color(yellow).

initial_board_possibilities([
[[red,green,blue,yellow],[red,green,blue,yellow],[red,green,blue,yellow]],
[[red,green,blue,yellow],[red,green,blue,yellow],[red,green,blue,yellow]],
[[red,green,blue,yellow],[red,green,blue,yellow],[red,green,blue,yellow]]]
).

board(Board) :-
	initial_board_possibilities(Initial),
	board(Board,Initial).
	
board(Board,Initial) :-
	board(Board,Initial,[]).
board(Board,[],Board)


block(Color,X,Y) :-
	pressable_color(Color).

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
	
query(initial_board(Board)).
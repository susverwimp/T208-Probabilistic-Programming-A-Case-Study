:- use_module(library(lists)).

pressable_color(red).
pressable_color(green).
pressable_color(blue).
pressable_color(yellow).

P::press(Board,X,Y,P) :-
	between(0,2,X),
	between(0,2,Y),
	member(block(Color,X,Y),Board),
	pressable_color(Color).
	
	
1/3::change_color(red,green);1/3::change_color(red,blue);1/3::change_color(red,yellow).
1/3::change_color(green,red);1/3::change_color(green,blue);1/3::change_color(green,yellow).
1/3::change_color(blue,red);1/3::change_color(blue,green);1/3::change_color(blue,yellow).
1/3::change_color(yellow,red);1/3::change_color(yellow,green);1/3::change_color(yellow,blue).

board(0,[[block(red,0,0),block(green,0,1),block(blue,0,2)],[block(yellow,1,0),block(yellow,1,1),block(red,1,2)],[block(red,2,0),block(green,2,1),block(red,2,2)]]).
board(T,Board) :-
	T > 0,
	TT is T - 1,
	board(TT,PreviousBoard),
	findall([WhiteX,WhiteY],member(block(white,WhiteX,WhiteY),PreviousBoard),WhitePositions),
	length(WhitePositions,L),
	PressProbs is 1/(9-L),
	press(PreviousBoard,X,Y,PressProbs),
	member(block(Color,X,Y),PreviousBoard),
	change_color(Color,NewColor),
	Position is Y*3+X,
	change_color_in_board(PreviousBoard,Position,NewColor,Board).
	
change_color_in_board(Board,Position,NewColor,NewBoard) :-
	change_color_in_board(Board,Position,NewColor,NewBoard,[]).
change_color_in_board([],_,_,NewBoard,NewBoard).
change_color_in_board([block(_,X,Y)|Tail],Position,NewColor,NewBoard,NewBoardAcc) :-
	Position2 is Y*3+X,
	Position = Position2,
	append(NewBoardAcc, [block(NewColor, X, Y)|Tail], NewNewBoardAcc),
	change_color_in_board([], X, NewColor, NewBoard, NewNewBoardAcc).
change_color_in_board([block(Color,X,Y)|Tail],Position,NewColor,NewBoard,NewBoardAcc) :-
	Position2 is Y*3+X,
	Position \= Position2,
	insert_at_end(block(Color, X, Y), NewBoardAcc, NewNewBoardAcc),
	change_color_in_board(Tail, Position, NewColor, NewBoard, NewNewBoardAcc).
	
insert_at_end(X,[],[X]).
insert_at_end(X,[H|T],[H|Z]) :- insert_at_end(X,T,Z).
	
score_of_turn(T,Score) :-
	board(T,Board),
	score(Board,Score).
	
score(Board,Score) :-
	findall([WhiteX,WhiteY],member(block(white,WhiteX,WhiteY),Board),WhitePositions),
	length(WhitePositions,Score).

% query(board(1,Board)).
query(score_of_turn(1,Score)).
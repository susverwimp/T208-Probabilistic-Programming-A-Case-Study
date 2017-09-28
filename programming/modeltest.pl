:- use_module(library(lists)).

probs_color_change(red, [0, 1/3, 1/3, 1/3]).
probs_color_change(green, [1/3, 0, 1/3, 1/3]).
probs_color_change(blue, [1/3, 1/3, 0, 1/3]).
probs_color_change(yellow, [1/3, 1/3, 1/3, 0]).

color(red).
color(green).
color(blue).
color(yellow).

% test 4x4 grid
%
% r g b r
% g y r g
% g r g y
% r g b r
%grid(red, 		1, 1).
%grid(green, 	2, 1).
%grid(blue, 		3, 1).
%grid(red, 		4, 1).

%grid(green, 	1, 2).
%grid(yellow, 	2, 2).
%grid(red, 		3, 2).
%grid(green, 	4, 2).

%grid(green, 	1, 3).
%grid(red, 		2, 3).
%grid(green, 	3, 3).
%grid(yellow, 	4, 3).

%grid(red, 		1, 4).
%grid(green, 	2, 4).
%grid(blue, 		3, 4).
%grid(red, 		4, 4).

%%%%%%%%%%%%%%%
% RANDOM EVENTS
%%%%%%%%%%%%%%%
% we'll press one of the blocks in the grid
%1/16::press(Color, X, Y) :- grid(Color, X, Y).
1/16::press(Grid, Color, X, Y) :- select(block(Color, X, Y), Grid, _).
% if we press a block, the color of the block is changed.
change_color(Grid, X, Y, NewColor, NewGrid) :-
	press(Grid, Color, X, Y),
	findall(C, color(C), Colors),
	probs_color_change(Color, Probs),
	select_weighted(1, Probs, Colors, NewColor, _),
	append(Grid, [block(NewColor, X, Y)], NewGrid).
	
change_grid(NewColor,X, Y,[], NewGrid, NewGrid).
change_grid(NewColor, X, Y, [Block|Tail], NewGrid, Acc).
%%%%%%%
% RULES
%%%%%%%

% when do we get points


game(X, Y, Score, NewColor, NewGrid) :-
	Grid = [
		block(red, 1, 1), block(green, 2, 1), block(blue, 3, 1), block(red, 4, 1),
		block(green, 1, 2), block(yellow, 2, 2), block(red, 3, 2), block(green, 4, 2),
		block(green, 1, 3), block(red, 2, 3), block(green, 3, 3), block(blue, 4, 3),
		block(red, 1, 4), block(green, 2, 4), block(blue, 3, 4), block(red, 4, 4)
	],
	turn1(Grid, X, Y, Score, NewColor, NewGrid).

turn1(Grid, X, Y, Score, NewColor, NewGrid) :-
	change_color(Grid, X, Y, NewColor, NewGrid),
	remove_blocks(Grid, X, Y, Score, NewColor),
	Score = 5.
	
remove_blocks(Grid, X, Y, Score, NewColor).


query(game(X, Y, Points, NewColor, NewGrid)).
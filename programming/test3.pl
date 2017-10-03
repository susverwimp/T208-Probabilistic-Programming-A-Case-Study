:- use_module(library(apply)).
:- use_module(library(lists)).

color(red).
color(green).
color(blue).
color(yellow).
no_color(nocolor).

%%%%%%%%%%%%%%%%%
% UTILITY PREDICATES
%%%%%%%%%%%%%%%%%

transpose(Ls, Ts) :-
	lists_transpose(Ls, Ts).

lists_transpose([], []).
lists_transpose([L|Ls], Ts) :-
	maplist(same_length(L), Ls),
	foldl(transpose_, L, Ts, [L|Ls], _).

transpose_(_, Fs, Lists0, Lists) :-
	maplist(list_first_rest, Lists0, Fs, Lists).
	
list_first_rest([L|Ls], L, Ls).

same_length([],[]).
same_length([_|L1],[_|L2]) :- same_length(L1, L2).


%%%%%%%%%%%%%%%
% RANDOM EVENTS
%%%%%%%%%%%%%%%
probs_color_change(red, [0, 1/3, 1/3, 1/3]).
probs_color_change(green, [1/3, 0, 1/3, 1/3]).
probs_color_change(blue, [1/3, 1/3, 0, 1/3]).
probs_color_change(yellow, [1/3, 1/3, 1/3, 0]).

% we'll press one of the blocks on board.
ProbPress::press(Board, X, Y, Color) :-
	how_many_blocks_with_color(Board, N),
	ProbPress is 1 / N,
	find_block(Board, X, Y, block(Color, X, Y)),
	color(Color).
	
change_color(Board, X, Y, NewColor, NewBoard) :-
	press(Board, X, Y, Color),
	findall(C, color(C), Colors),
	probs_color_change(Color, Probs),
	select_weighted(1, Probs, Colors, NewColor, _),
	NewBoard is 5.
	
find_block([[Block|_]|_], 1, 1, Block).
find_block([Row|Tail], X, Y, Block) :-
	Y > 1,
	NewY is Y - 1,
	find_block(Tail, X, NewY, Block).
find_block([[Block|Tail]|Tail2], X, 1, Acc) :-
	X > 1,
	NewX is X - 1,
	find_block([Tail1|Tail2]).



how_many_blocks_with_color(Board, Blocks) :-
	how_many_blocks_with_color(Board, Blocks, 0).
how_many_blocks_with_color([], Blocks, Blocks).
how_many_blocks_with_color([[]|Tail], Blocks, Acc) :-
	how_many_blocks_with_color(Tail, Blocks, Acc).
how_many_blocks_with_color([[block(Color, _, _)|Tail1]|Tail2], Blocks, Acc) :-
	color(Color),
	NewAcc is Acc + 1,
	how_many_blocks_with_color([Tail1|Tail2], Blocks, NewAcc).
how_many_blocks_with_color([[block(Color, _, _)|Tail1]|Tail2], Blocks, Acc) :-
	no_color(Color),
	how_many_blocks_with_color([Tail1|Tail2], Blocks, Acc).
	

%%%%%%%%%%%%%%%%%
% GAME PREDICATES
%%%%%%%%%%%%%%%%%
create_board(Width, Height, Board) :-
	create_board(Width, Height, Board, [], Width, Height, []).
create_board(_, _, Board, Board, _, 0, _).
create_board(Width, Height, Board, Acc, 1, HeightAcc, RowAcc) :-
	HeightAcc > 0,
	color(Color),
	NewAcc = [[block(Color, 1, HeightAcc)|RowAcc]|Acc],
	NewWidthAcc is Width,
	NewHeightAcc is HeightAcc - 1,
	create_board(Width, Height, Board, NewAcc, NewWidthAcc, NewHeightAcc, []).
create_board(Width, Height, Board, Acc, WidthAcc, HeightAcc, RowAcc) :-
	WidthAcc > 1,
	HeightAcc > 0,
	color(Color),
	NewRowAcc = [block(Color, WidthAcc, HeightAcc)|RowAcc],
	NewWidthAcc is WidthAcc - 1,
	create_board(Width, Height, Board, Acc, NewWidthAcc, HeightAcc, NewRowAcc).
	
find_same_horizontal(Board, Same) :-
	find_same_horizontal(Board, Same, []).
find_same_horizontal([], Same, Same).
find_same_horizontal([Row|Tail], Same, Acc) :-
	find_same_in_list(Row, SameInRow),
	append(SameInRow, Acc, NewAcc),
	find_same_horizontal(Tail, Same, NewAcc).
	
find_same_vertical(Board, Same) :-
	transpose(Board, ColumnBoard),
	find_same_horizontal(ColumnBoard, Same, []).
	
find_same_in_list(List, Same) :-
	find_same_in_list(List, Same, [], []).
find_same_in_list([], Same, Acc, TotalAcc) :-
	length(Acc, Length),
	Length > 2,
	append(Acc, TotalAcc, Same).
find_same_in_list([], Same, Acc, TotalAcc) :-
	length(Acc, Length),
	Length =< 2,
	Same = TotalAcc.
find_same_in_list([Block|Tail], Same, [], TotalAcc) :-
	find_same_in_list(Tail, Same, [Block], TotalAcc).
find_same_in_list([block(Color1, X1, Y1)|Tail], Same, [block(Color2, X2, Y2)|TailAcc], TotalAcc) :-
	Color1 == Color2,
	NewAcc = [block(Color1, X1, Y1),block(Color2, X2, Y2)|TailAcc],
	find_same_in_list(Tail, Same, NewAcc, TotalAcc).
find_same_in_list([block(Color1, X1, Y1)|Tail], Same, [block(Color2, X2, Y2)|TailAcc], Acc) :-
	Color1 \= Color2,
	length([block(Color2, X2, Y2)|TailAcc], Length),
	Length > 2,
	append([block(Color2, X2, Y2)|TailAcc], Acc, NewAcc),
	find_same_in_list(Tail, Same, [block(Color1, X1, Y1)], NewAcc).
find_same_in_list([block(Color1, X1, Y1)|Tail], Same, [block(Color2, X2, Y2)|TailAcc], Acc) :-
	Color1 \= Color2,
	length([block(Color2, X2, Y2)|TailAcc], Length),
	Length =< 2,
	find_same_in_list(Tail, Same, [block(Color1, X1, Y1)], Acc).

start(Board, Width, Height) :-
	%create_board(Width, Height, Board),
	Board = [
		[block(red, 1, 1), block(green, 2, 1), block(blue, 3, 1), block(red, 4, 1), block(green, 5, 1), block(blue, 6, 1), block(red, 7, 1), block(green, 8, 1), block(blue, 9, 1), block(red, 10, 1)],
		[block(green, 1, 2), block(blue, 2, 2), block(red, 3, 2), block(green, 4, 2), block(blue, 5, 2), block(red, 6, 2), block(green, 7, 2), block(blue, 8, 2), block(red, 9, 2), block(green, 10, 2)],
		[block(blue, 1, 3), block(red, 2, 3), block(green, 3, 3), block(blue, 4, 3), block(red, 5, 3), block(green, 6, 3), block(blue, 7, 3), block(red, 8, 3), block(green, 9, 3), block(blue, 10, 3)],
		[block(red, 1, 4), block(green, 2, 4), block(blue, 3, 4), block(red, 4, 4), block(green, 5, 4), block(blue, 6, 4), block(red, 7, 4), block(green, 8, 4), block(blue, 9, 4), block(red, 10, 4)],
		[block(green, 1, 5), block(blue, 2, 5), block(red, 3, 5), block(green, 4, 5), block(blue, 5, 5), block(red, 6, 5), block(green, 7, 5), block(blue, 8, 5), block(red, 9, 5), block(green, 10, 5)],
		[block(blue, 1, 6), block(red, 2, 6), block(green, 3, 6), block(blue, 4, 6), block(red, 5, 6), block(green, 6, 6), block(blue, 7, 6), block(red, 8, 6), block(green, 9, 6), block(blue, 10, 6)],
		[block(red, 1, 7), block(green, 2, 7), block(blue, 3, 7), block(red, 4, 7), block(green, 5, 7), block(blue, 6, 7), block(red, 7, 7), block(green, 8, 7), block(blue, 9, 7), block(red, 10, 7)],
		[block(green, 1, 8), block(blue, 2, 8), block(red, 3, 8), block(green, 4, 8), block(blue, 5, 8), block(red, 6, 8), block(green, 7, 8), block(blue, 8, 8), block(red, 9, 8), block(green, 10, 8)],
		[block(blue, 1, 9), block(red, 2, 9), block(green, 3, 9), block(blue, 4, 9), block(red, 5, 9), block(green, 6, 9), block(blue, 7, 9), block(red, 8, 9), block(green, 9, 9), block(blue, 10, 9)],
		[block(red, 1, 10), block(green, 2, 10), block(blue, 3, 10), block(red, 4, 10), block(green, 5, 10), block(blue, 6, 10), block(red, 7, 10), block(green, 8, 10), block(blue, 9, 10), block(red, 10, 10)]
	],
	find_same_horizontal(Board, []),
	find_same_vertical(Board, []).
	
	
query(change_color([
		[block(red, 1, 1), block(green, 2, 1), block(blue, 3, 1), block(red, 4, 1), block(green, 5, 1), block(blue, 6, 1), block(red, 7, 1), block(green, 8, 1), block(blue, 9, 1), block(red, 10, 1)],
		[block(green, 1, 2), block(blue, 2, 2), block(red, 3, 2), block(green, 4, 2), block(blue, 5, 2), block(red, 6, 2), block(green, 7, 2), block(blue, 8, 2), block(red, 9, 2), block(green, 10, 2)],
		[block(blue, 1, 3), block(red, 2, 3), block(green, 3, 3), block(blue, 4, 3), block(red, 5, 3), block(green, 6, 3), block(blue, 7, 3), block(red, 8, 3), block(green, 9, 3), block(blue, 10, 3)],
		[block(red, 1, 4), block(green, 2, 4), block(blue, 3, 4), block(red, 4, 4), block(green, 5, 4), block(blue, 6, 4), block(red, 7, 4), block(green, 8, 4), block(blue, 9, 4), block(red, 10, 4)],
		[block(green, 1, 5), block(blue, 2, 5), block(red, 3, 5), block(green, 4, 5), block(blue, 5, 5), block(red, 6, 5), block(green, 7, 5), block(blue, 8, 5), block(red, 9, 5), block(green, 10, 5)],
		[block(blue, 1, 6), block(red, 2, 6), block(green, 3, 6), block(blue, 4, 6), block(red, 5, 6), block(green, 6, 6), block(blue, 7, 6), block(red, 8, 6), block(green, 9, 6), block(blue, 10, 6)],
		[block(red, 1, 7), block(green, 2, 7), block(blue, 3, 7), block(red, 4, 7), block(green, 5, 7), block(blue, 6, 7), block(red, 7, 7), block(green, 8, 7), block(blue, 9, 7), block(red, 10, 7)],
		[block(green, 1, 8), block(blue, 2, 8), block(red, 3, 8), block(green, 4, 8), block(blue, 5, 8), block(red, 6, 8), block(green, 7, 8), block(blue, 8, 8), block(red, 9, 8), block(green, 10, 8)],
		[block(blue, 1, 9), block(red, 2, 9), block(green, 3, 9), block(blue, 4, 9), block(red, 5, 9), block(green, 6, 9), block(blue, 7, 9), block(red, 8, 9), block(green, 9, 9), block(blue, 10, 9)],
		[block(red, 1, 10), block(green, 2, 10), block(blue, 3, 10), block(red, 4, 10), block(green, 5, 10), block(blue, 6, 10), block(red, 7, 10), block(green, 8, 10), block(blue, 9, 10), block(red, 10, 10)]
	], X, Y, NewColor, NewBoard)).
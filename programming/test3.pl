:- use_module(library(apply)).
:- use_module(library(lists)).

color(red).
color(green).
color(blue).
color(yellow).

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

insert_at_end(X,[],[X]).
insert_at_end(X,[H|T],[H|Z]) :- insert_at_end(X,T,Z).

pack([],[]).
pack([X|Xs],[Z|Zs]) :- transfer(X,Xs,Ys,Z), pack(Ys,Zs).

transfer(X,[],[],[X]).
transfer(block(Color1,X1,Y1),[block(Color2,X2,Y2)|Ys],[block(Color2,X2,Y2)|Ys],[block(Color1,X1,Y1)]) :- Color1 \= Color2.
transfer(block(Color,X1,Y1),[block(Color,X2,Y2)|Xs],Ys,[block(Color,X1,Y1)|Zs]) :- transfer(block(Color,X2,Y2),Xs,Ys,Zs).

set([], []).
set([H|T], [H|T1]) :- 
    remv(H, T, T2),
    set(T2, T1).

remv(_, [], []).
remv(X, [X|T], T1) :- remv(X, T, T1).
remv(X, [H|T], [H|T1]) :-
    X \= H,
    remv(X, T, T1).

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
	find_color_of_block(Board, X, Y, Color),
	color(Color).
	
change_color(Board, X, Y, NewColor, Score) :-
	press(Board, X, Y, Color),
	findall(C, color(C), Colors),
	probs_color_change(Color, Probs),
	select_weighted(1, Probs, Colors, NewColor, _),
	change_color_in_board(Board, X, Y, NewColor, NewBoard),
	remove_and_drop(NewBoard, NewNewBoard, Score).

remove_and_drop(Board, NewBoard, Score) :-
	remove_and_drop(Board, NewBoard, Score, 0).
remove_and_drop(Board, Board, Score, Score) :-
	find_same(Board, []).
remove_and_drop(Board, NewBoard, Score, ScoreAcc) :-
	find_same(Board, Same),
	length(Same, Length),
	Length > 0,
	NewScoreAcc is ScoreAcc + Length,
	remove_same(Board, Same, BoardAcc),
	drop_blocks(BoardAcc, NewBoardAcc),
	remove_and_drop(NewBoardAcc, NewBoard, Score, NewScoreAcc).
	
remove_same(Board, [], Board).
remove_same(Board, [block(_, X, Y)|Tail], NewBoard) :-
	change_color_in_board(Board, X, Y, nocolor, BoardAcc),
	remove_same(BoardAcc, Tail, NewBoard).
	
drop_blocks(Board, NewBoard) :-
	transpose(Board, Transpose),
	drop_columns(Transpose, NewBoardTranspose),
	transpose(NewBoardTranspose, NewBoard).
	
	
drop_columns(Board, NewBoard) :-
	drop_columns(Board, NewBoard, [], 1).
drop_columns([], NewBoard, NewBoard, _).
drop_columns([Column|Tail], NewBoard, NewBoardAcc, ColumnIndex) :-
	get_no_color_and_colors_of_column(Column, NoColors, Colors),
	append(NoColors, Colors, NewColorColumn),
	create_column_from_color_list(NewColorColumn, NewColumn, ColumnIndex),
	NewColumnIndex is ColumnIndex + 1,
	append(NewBoardAcc, [NewColumn], NewNewBoardAcc),
	drop_columns(Tail, NewBoard, NewNewBoardAcc, NewColumnIndex).

create_column_from_color_list(ColorColumn, NewColumn, ColumnIndex) :-
	create_column_from_color_list(ColorColumn, NewColumn, [], 1, ColumnIndex).
create_column_from_color_list([], Column, Column, _, _).
create_column_from_color_list([Color|Tail], Column, ColumnAcc, RowIndex, ColumnIndex) :-
	append(ColumnAcc, [block(Color, ColumnIndex, RowIndex)], NewColumnAcc),
	NewRowIndex is RowIndex + 1,
	create_column_from_color_list(Tail, Column, NewColumnAcc, NewRowIndex, ColumnIndex).

	
get_no_color_and_colors_of_column(Column, NoColors, Colors) :-
	get_no_color_and_colors_of_column(Column, NoColors, Colors, [], []).
get_no_color_and_colors_of_column([], NoColors, Colors, NoColors, Colors).
get_no_color_and_colors_of_column([block(Color, _, _)|Tail], NoColors, Colors, NoColorsAcc, ColorsAcc) :-
	color(Color),
	append(ColorsAcc, [Color], NewColorsAcc),
	get_no_color_and_colors_of_column(Tail, NoColors, Colors, NoColorsAcc, NewColorsAcc).
get_no_color_and_colors_of_column([block(Color, _, _)|Tail], NoColors, Colors, NoColorsAcc, ColorsAcc) :-
	\+color(Color),
	append(NoColorsAcc, [Color], NewNoColorsAcc),
	get_no_color_and_colors_of_column(Tail, NoColors, Colors, NewNoColorsAcc, ColorsAcc).
	
find_color_of_block(Board, X, Y, Color) :-
	member(Row, Board),
	member(block(Color, X, Y), Row).
	
change_color_in_board(Board, X, Y, NewColor, NewBoard) :-
	change_color_in_board(Board, X, Y, NewColor, NewBoard, []).
change_color_in_board([], _, _, _, NewBoard, NewBoard).
change_color_in_board([Row|Tail], X, Y, NewColor, NewBoard, NewBoardAcc) :-
	\+member(block(Color, X, Y), Row),
	append(NewBoardAcc,[Row], NewNewBoardAcc),
	change_color_in_board(Tail, X, Y, NewColor, NewBoard, NewNewBoardAcc).
change_color_in_board([Row|Tail], X, Y, NewColor, NewBoard, NewBoardAcc) :-
	member(block(Color, X, Y), Row),
	change_color_in_row(Row, X, NewColor, NewRow),
	append(NewBoardAcc,[NewRow|Tail], NewNewBoardAcc),
	change_color_in_board([], X, Y, NewColor, NewBoard, NewNewBoardAcc).
	
change_color_in_row(Row, X, NewColor, NewRow) :-
	change_color_in_row(Row, X, NewColor, NewRow, []).
change_color_in_row([], _, _, NewRow, NewRow).
change_color_in_row([block(_,X,Y)|Tail], X, NewColor, NewRow, NewRowAcc) :-
	append(NewRowAcc, [block(NewColor, X, Y)|Tail], NewNewRowAcc),
	change_color_in_row([], X, NewColor, NewRow, NewNewRowAcc).
change_color_in_row([block(Color,X1,Y)|Tail], X2, NewColor, NewRow, NewRowAcc) :-
	X1 \= X2,
	insert_at_end(block(Color, X1, Y), NewRowAcc, NewNewRowAcc),
	change_color_in_row(Tail, X2, NewColor, NewRow, NewNewRowAcc).


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
	\+color(Color),
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
	
find_same(Board, Same) :-
	find_same_horizontal(Board, Horizontal),
	find_same_vertical(Board, Vertical),
	append(Horizontal, Vertical, SameAcc),
	set(SameAcc, Same).
	
find_same_horizontal(Board, Same) :-
	find_same_horizontal(Board, Same, []).
find_same_horizontal([], Same, Same).
find_same_horizontal([Row|Tail], Same, Acc) :-
	find_same_in_list(Row, SameInRow),
	append(Acc, SameInRow, NewAcc),
	find_same_horizontal(Tail, Same, NewAcc).
	
find_same_vertical(Board, Same) :-
	transpose(Board, ColumnBoard),
	find_same_horizontal(ColumnBoard, Same, []).
	
find_same_in_list(List, Same) :-
	pack(List, PackedList),
	find_same_in_packed_list(PackedList, Same, []).
find_same_in_packed_list([], Same, Same).
find_same_in_packed_list([Pack|Tail], Same, Acc) :-
	length(Pack, Length),
	Length > 2,
	append(Acc, Pack, NewAcc),
	find_same_in_packed_list(Tail, Same, NewAcc).
find_same_in_packed_list([Pack|Tail], Same, Acc) :-
	length(Pack, Length),
	Length =< 2,
	find_same_in_packed_list(Tail, Same, Acc).

start(Board, Width, Height) :-
	create_board(Width, Height, Board),
%	Board = [
%		[block(red, 1, 1), block(green, 2, 1), block(blue, 3, 1), block(red, 4, 1), block(green, 5, 1), block(blue, 6, 1), block(red, 7, 1), block(green, 8, 1), block(blue, 9, 1), block(red, 10, 1)],
%		[block(green, 1, 2), block(blue, 2, 2), block(red, 3, 2), block(green, 4, 2), block(blue, 5, 2), block(red, 6, 2), block(green, 7, 2), block(blue, 8, 2), block(red, 9, 2), block(green, 10, 2)],
%		[block(blue, 1, 3), block(red, 2, 3), block(green, 3, 3), block(blue, 4, 3), block(red, 5, 3), block(green, 6, 3), block(blue, 7, 3), block(red, 8, 3), block(green, 9, 3), block(blue, 10, 3)],
%		[block(red, 1, 4), block(green, 2, 4), block(blue, 3, 4), block(red, 4, 4), block(green, 5, 4), block(blue, 6, 4), block(red, 7, 4), block(green, 8, 4), block(blue, 9, 4), block(red, 10, 4)],
%		[block(green, 1, 5), block(blue, 2, 5), block(red, 3, 5), block(green, 4, 5), block(blue, 5, 5), block(red, 6, 5), block(green, 7, 5), block(blue, 8, 5), block(red, 9, 5), block(green, 10, 5)],
%		[block(blue, 1, 6), block(red, 2, 6), block(green, 3, 6), block(blue, 4, 6), block(red, 5, 6), block(green, 6, 6), block(blue, 7, 6), block(red, 8, 6), block(green, 9, 6), block(blue, 10, 6)],
%		[block(red, 1, 7), block(green, 2, 7), block(blue, 3, 7), block(red, 4, 7), block(green, 5, 7), block(blue, 6, 7), block(red, 7, 7), block(green, 8, 7), block(blue, 9, 7), block(red, 10, 7)],
%		[block(green, 1, 8), block(blue, 2, 8), block(red, 3, 8), block(green, 4, 8), block(blue, 5, 8), block(red, 6, 8), block(green, 7, 8), block(blue, 8, 8), block(red, 9, 8), block(green, 10, 8)],
%		[block(blue, 1, 9), block(red, 2, 9), block(green, 3, 9), block(blue, 4, 9), block(red, 5, 9), block(green, 6, 9), block(blue, 7, 9), block(red, 8, 9), block(green, 9, 9), block(blue, 10, 9)],
%		[block(red, 1, 10), block(green, 2, 10), block(blue, 3, 10), block(red, 4, 10), block(green, 5, 10), block(blue, 6, 10), block(red, 7, 10), block(green, 8, 10), block(blue, 9, 10), block(red, 10, 10)]
%	],
	find_same_horizontal(Board, []),
	find_same_vertical(Board, []).
	

	
%query(change_color([
%		[block(red, 1, 1), block(green, 2, 1), block(blue, 3, 1), block(red, 4, 1), block(green, 5, 1), block(blue, 6, 1), block(red, 7, 1), block(green, 8, 1), block(blue, 9, 1), block(red, 10, 1)],
%		[block(green, 1, 2), block(blue, 2, 2), block(red, 3, 2), block(green, 4, 2), block(blue, 5, 2), block(red, 6, 2), block(green, 7, 2), block(blue, 8, 2), block(red, 9, 2), block(green, 10, 2)],
%		[block(blue, 1, 3), block(red, 2, 3), block(green, 3, 3), block(blue, 4, 3), block(red, 5, 3), block(green, 6, 3), block(blue, 7, 3), block(red, 8, 3), block(green, 9, 3), block(blue, 10, 3)],
%		[block(red, 1, 4), block(green, 2, 4), block(blue, 3, 4), block(red, 4, 4), block(green, 5, 4), block(blue, 6, 4), block(red, 7, 4), block(green, 8, 4), block(blue, 9, 4), block(red, 10, 4)],
%		[block(green, 1, 5), block(blue, 2, 5), block(red, 3, 5), block(green, 4, 5), block(blue, 5, 5), block(red, 6, 5), block(green, 7, 5), block(blue, 8, 5), block(red, 9, 5), block(green, 10, 5)],
%		[block(blue, 1, 6), block(red, 2, 6), block(green, 3, 6), block(blue, 4, 6), block(red, 5, 6), block(green, 6, 6), block(blue, 7, 6), block(red, 8, 6), block(green, 9, 6), block(blue, 10, 6)],
%		[block(red, 1, 7), block(green, 2, 7), block(blue, 3, 7), block(red, 4, 7), block(green, 5, 7), block(blue, 6, 7), block(red, 7, 7), block(green, 8, 7), block(blue, 9, 7), block(red, 10, 7)],
%		[block(green, 1, 8), block(blue, 2, 8), block(red, 3, 8), block(green, 4, 8), block(blue, 5, 8), block(red, 6, 8), block(green, 7, 8), block(blue, 8, 8), block(red, 9, 8), block(green, 10, 8)],
%		[block(blue, 1, 9), block(red, 2, 9), block(green, 3, 9), block(blue, 4, 9), block(red, 5, 9), block(green, 6, 9), block(blue, 7, 9), block(red, 8, 9), block(green, 9, 9), block(blue, 10, 9)],
%		[block(red, 1, 10), block(green, 2, 10), block(blue, 3, 10), block(red, 4, 10), block(green, 5, 10), block(blue, 6, 10), block(red, 7, 10), block(green, 8, 10), block(blue, 9, 10), block(red, 10, 10)]
%	], X, Y, Color, Score)).
	
query(change_color([
		[block(red, 1, 1), block(red, 2, 1), block(blue, 3, 1)],
		[block(green, 1, 2), block(blue, 2, 2), block(red, 3, 2)],
		[block(red, 1, 3), block(red, 2, 3), block(green, 3, 3)]
	], 1, 2, Color, Score)).

%query(find_same([
%		[block(red, 1, 1), block(red, 2, 1), block(red, 3, 1), block(red, 4, 1), block(green, 5, 1), block(blue, 6, 1), block(red, 7, 1), block(green, 8, 1), block(blue, 9, 1), block(red, 10, 1)],
%		[block(green, 1, 2), block(blue, 2, 2), block(red, 3, 2), block(green, 4, 2), block(blue, 5, 2), block(red, 6, 2), block(green, 7, 2), block(blue, 8, 2), block(red, 9, 2), block(green, 10, 2)],
%		[block(blue, 1, 3), block(blue, 2, 3), block(blue, 3, 3), block(blue, 4, 3), block(red, 5, 3), block(green, 6, 3), block(blue, 7, 3), block(red, 8, 3), block(green, 9, 3), block(blue, 10, 3)],
%		[block(red, 1, 4), block(green, 2, 4), block(blue, 3, 4), block(red, 4, 4), block(green, 5, 4), block(blue, 6, 4), block(red, 7, 4), block(green, 8, 4), block(blue, 9, 4), block(red, 10, 4)],
%		[block(green, 1, 5), block(blue, 2, 5), block(red, 3, 5), block(green, 4, 5), block(blue, 5, 5), block(red, 6, 5), block(green, 7, 5), block(blue, 8, 5), block(red, 9, 5), block(green, 10, 5)],
%		[block(blue, 1, 6), block(red, 2, 6), block(green, 3, 6), block(blue, 4, 6), block(red, 5, 6), block(green, 6, 6), block(blue, 7, 6), block(red, 8, 6), block(green, 9, 6), block(blue, 10, 6)],
%		[block(red, 1, 7), block(green, 2, 7), block(blue, 3, 7), block(red, 4, 7), block(green, 5, 7), block(blue, 6, 7), block(red, 7, 7), block(green, 8, 7), block(blue, 9, 7), block(red, 10, 7)],
%		[block(green, 1, 8), block(blue, 2, 8), block(red, 3, 8), block(green, 4, 8), block(blue, 5, 8), block(red, 6, 8), block(green, 7, 8), block(blue, 8, 8), block(red, 9, 8), block(green, 10, 8)],
%		[block(blue, 1, 9), block(red, 2, 9), block(green, 3, 9), block(blue, 4, 9), block(red, 5, 9), block(green, 6, 9), block(blue, 7, 9), block(red, 8, 9), block(green, 9, 9), block(blue, 10, 9)],
%		[block(red, 1, 10), block(green, 2, 10), block(blue, 3, 10), block(red, 4, 10), block(green, 5, 10), block(blue, 6, 10), block(red, 7, 10), block(green, 8, 10), block(blue, 9, 10), block(red, 10, 10)]
%	], Same)).

%query(change_color_in_row([block(red, 1, 1), block(green, 2, 1), block(blue, 3, 1), block(red, 4, 1), block(green, 5, 1), block(blue, 6, 1), block(red, 7, 1), block(green, 8, 1), block(blue, 9, 1), block(red, 10, 1)], 1, green, NewRow)).
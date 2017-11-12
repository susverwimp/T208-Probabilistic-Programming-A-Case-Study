:- use_module(library(apply)).
:- use_module(library(lists)).

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pressable_color(red).
pressable_color(green).
pressable_color(blue).
pressable_color(yellow).

1/3::strategy(uniform);1/3::strategy(possible_score);1/3::strategy(color_ratio).

P::press(Board,X,Y,T,uniform) :-
	find_block_in_board(block(Color,X,Y),Board),
	pressable_color(Color),
	how_many_blocks_with_color(Board, N),
	P is 1 / N.
	
P::press(Board,X,Y,T,color_ratio) :-
	find_block_in_board(block(Color,X,Y),Board),
	flatten(Board,FlattenBoard),
	list_ratio(FlattenBoard,ColorRatio),
	min_ratio(ColorRatio,[MinColor,MinCount]),
	Color = MinColor,
	P is 1 / MinCount.
	
P::press(Board,X,Y,T,possible_score) :-
	find_block_in_board(block(Color,X,Y),Board),
	pressable_color(Color),
	how_many_blocks_with_color(Board, N),
	P is 1 / N.
	
flatten([],[]).
flatten([X|Xs],Zs) :- flatten(X,Y), flatten(Xs,Ys), append(Y,Ys,Zs).
	
list_ratio(List, ListRatio) :-
	list_ratio(List, ListRatio, 0, 0, 0, 0).
list_ratio([], [[red,RedAcc], [green,GreenAcc], [blue,BlueAcc], [yellow,YellowAcc]], RedAcc, GreenAcc, BlueAcc, YellowAcc).
list_ratio([block(Color,_,_)|Tail], ListRatio, RedAcc, GreenAcc, BlueAcc, YellowAcc) :-
	Color = red,
	NewRedAcc is RedAcc + 1,
	list_ratio(Tail, ListRatio, NewRedAcc, GreenAcc, BlueAcc, YellowAcc).
list_ratio([block(Color,_,_)|Tail], ListRatio, RedAcc, GreenAcc, BlueAcc, YellowAcc) :-
	Color = green,
	NewGreenAcc is GreenAcc + 1,
	list_ratio(Tail, ListRatio, RedAcc, NewGreenAcc, BlueAcc, YellowAcc).
list_ratio([block(Color,_,_)|Tail], ListRatio, RedAcc, GreenAcc, BlueAcc, YellowAcc) :-
	Color = blue,
	NewBlueAcc is BlueAcc + 1,
	list_ratio(Tail, ListRatio, RedAcc, GreenAcc, NewBlueAcc, YellowAcc).
list_ratio([block(Color,_,_)|Tail], ListRatio, RedAcc, GreenAcc, BlueAcc, YellowAcc) :-
	Color = yellow,
	NewYellowAcc is YellowAcc + 1,
	list_ratio(Tail, ListRatio, RedAcc, GreenAcc, BlueAcc, NewYellowAcc).
list_ratio([block(Color,_,_)|Tail], ListRatio, RedAcc, GreenAcc, BlueAcc, YellowAcc) :-
	Color = white,
	list_ratio(Tail, ListRatio, RedAcc, GreenAcc, BlueAcc, YellowAcc).
	
min_ratio([X|L],S) :- min_ratio(L,X,S).
min_ratio([],S,S).
min_ratio([[Color1,ColorCount1]|L],[Color2,ColorCount2],S) :-
    ColorCount2 < ColorCount1,
    min_ratio(L,[Color2,ColorCount2],S).
min_ratio([[Color1,ColorCount1]|L],[Color2,ColorCount2],S) :-
    ColorCount2 >= ColorCount1,
    min_ratio(L,[Color1,ColorCount1],S).
	
	
press(Board,X,Y,T) :-
	strategy(Strategy),
	press(Board,X,Y,T,Strategy).
	
how_many_blocks_with_color(Board, Blocks) :-
	how_many_blocks_with_color(Board, Blocks, 0).
how_many_blocks_with_color([], Blocks, Blocks).
how_many_blocks_with_color([[]|Tail], Blocks, Acc) :-
	how_many_blocks_with_color(Tail, Blocks, Acc).
how_many_blocks_with_color([[block(Color, _, _)|Tail1]|Tail2], Blocks, Acc) :-
	pressable_color(Color),
	NewAcc is Acc + 1,
	how_many_blocks_with_color([Tail1|Tail2], Blocks, NewAcc).
how_many_blocks_with_color([[block(Color, _, _)|Tail1]|Tail2], Blocks, Acc) :-
	\+ pressable_color(Color),
	how_many_blocks_with_color([Tail1|Tail2], Blocks, Acc).
	

1/3::change_color(red,green);1/3::change_color(red,blue);1/3::change_color(red,yellow).
1/3::change_color(green,red);1/3::change_color(green,blue);1/3::change_color(green,yellow).
1/3::change_color(blue,red);1/3::change_color(blue,green);1/3::change_color(blue,yellow).
1/3::change_color(yellow,red);1/3::change_color(yellow,green);1/3::change_color(yellow,blue).

board(0,[[block(red,0,0),block(green,1,0),block(blue,2,0)],[block(yellow,0,1),block(yellow,1,1),block(red,2,1)],[block(red,0,2),block(green,1,2),block(blue,2,2)]],0).
board(T,Board,X,Y,Score) :-
	T > 0,
	TT is T - 1,
	board(TT,PreviousBoard,PreviousScore),
	press(PreviousBoard,X,Y,TT),
	find_block_in_board(block(Color,X,Y),PreviousBoard),
	change_color(Color,NewColor),
	change_color_in_board(PreviousBoard,X,Y,NewColor,ColorChangedBoard),
	remove_and_drop(ColorChangedBoard, Board, CurrentScore),
	Score is PreviousScore + CurrentScore.
	
score_of_turn(T,Score) :-
	board(T,Board,X,Y,Score).
	
	
find_block_in_board(block(Color,X,Y),Board) :-
	member(Row, Board),
	member(block(Color, X, Y), Row).
	
change_color_in_board(Board, X, Y, NewColor, NewBoard) :-
	change_color_in_board(Board, X, Y, NewColor, NewBoard, []).
change_color_in_board([], _, _, _, NewBoard, NewBoard).
change_color_in_board([Row|Tail], X, Y, NewColor, NewBoard, NewBoardAcc) :-
	\+member(block(_, X, Y), Row),
	append(NewBoardAcc,[Row], NewNewBoardAcc),
	change_color_in_board(Tail, X, Y, NewColor, NewBoard, NewNewBoardAcc).
change_color_in_board([Row|Tail], X, Y, NewColor, NewBoard, NewBoardAcc) :-
	member(block(_, X, Y), Row),
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
	change_color_in_board(Board, X, Y, white, BoardAcc),
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
	pressable_color(Color),
	append(ColorsAcc, [Color], NewColorsAcc),
	get_no_color_and_colors_of_column(Tail, NoColors, Colors, NoColorsAcc, NewColorsAcc).
get_no_color_and_colors_of_column([block(Color, _, _)|Tail], NoColors, Colors, NoColorsAcc, ColorsAcc) :-
	\+ pressable_color(Color),
	append(NoColorsAcc, [Color], NewNoColorsAcc),
	get_no_color_and_colors_of_column(Tail, NoColors, Colors, NewNoColorsAcc, ColorsAcc).
	
find_color_of_block(Board, X, Y, Color) :-
	member(Row, Board),
	member(block(Color, X, Y), Row).
	
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
	remove_no_colors_from_packed_list(PackedList, NoColorPackedList),
	find_same_in_packed_list(NoColorPackedList, Same, []).
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

remove_no_colors_from_packed_list(PackedList, Result) :-
	remove_no_colors_from_packed_list(PackedList, Result, []).
remove_no_colors_from_packed_list([], Result, Result).
remove_no_colors_from_packed_list([[block(Color,X,Y)|Tail1]|Tail2], Result, ResultAcc) :-
	pressable_color(Color),
	append(ResultAcc, [[block(Color,X,Y)|Tail1]], NewResultAcc),
	remove_no_colors_from_packed_list(Tail2, Result, NewResultAcc).
remove_no_colors_from_packed_list([[block(Color,_,_)|_]|Tail2], Result, ResultAcc) :-
	\+ pressable_color(Color),
	remove_no_colors_from_packed_list(Tail2, Result, ResultAcc).
	
evidence(strategy(uniform),false).
evidence(strategy(possible_score),false).
evidence(strategy(color_ratio),true).
	
% query(board(1,Board,X,Y,Score)).
query(score_of_turn(1,Score)).
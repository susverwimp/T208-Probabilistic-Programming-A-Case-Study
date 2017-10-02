color(red).
color(green).
%color(blue).
%color(yellow).

%%%%%%%%%%%%%%%%%
% UTILITY PREDICATES
%%%%%%%%%%%%%%%%%
append([],L,L). 
append([H|T],L2,[H|L3])  :-  append(T,L2,L3).

transpose(Ls, Ts) :-
	lists_transpose(Ls, Ts).

lists_transpose([], []).
lists_transpose([L|Ls], Ts) :-
	maplist(same_length(L), Ls),
	foldl(transpose_, L, Ts, [L|Ls], _).

transpose_(_, Fs, Lists0, Lists) :-
	maplist(list_first_rest, Lists0, Fs, Lists).
	
list_first_rest([L|Ls], L, Ls).

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
	create_board(Width, Height, Board),
	find_same_horizontal(Board, []),
	find_same_vertical(Board, []).
	
	
query(create_board(2, 2, Board)).
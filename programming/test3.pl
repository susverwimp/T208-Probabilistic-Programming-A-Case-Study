color(red).
color(green).
color(blue).
color(yellow).

%%%%%%%%%%%%%%%%%
% UTILITY PREDICATES
%%%%%%%%%%%%%%%%%
append([],L,L). 
append([H|T],L2,[H|L3])  :-  append(T,L2,L3).

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

	
find_same_in_list(List, Same) :-
	find_same_in_list(List, Same, [], 0).
find_same_in_list([], Same, Same, Length).
find_same_in_list([Block|Tail], Same, Acc, Length) :-
	
	find_same_in_list(Tail, Same, Acc).
	

game(Board, Width, Height) :-
	create_board(Width, Height, Board),
	find_same_horizontal(Board, []),
	find_same_vertical(Board, []).
	
query(create_board(2, 2, Board)).
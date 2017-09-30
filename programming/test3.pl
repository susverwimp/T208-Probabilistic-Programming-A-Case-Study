color(red).
color(green).
color(blue).
color(yellow).

append([],L,L). 
append([H|T],L2,[H|L3])  :-  append(T,L2,L3).

create_board(Width, Height, Board) :-
	create_board(Width, Height, Board, [], Width, Height).
create_board(_, _, Board, Board, _, 0).
create_board(Width, Height, Board, Acc, 1, HeightAcc) :-
	HeightAcc > 0,
	color(Color),
	append([block(Color, 1, HeightAcc)], Acc, NewAcc),
	NewWidthAcc is Width,
	NewHeightAcc is HeightAcc - 1,
	create_board(Width, Height, Board, NewAcc, NewWidthAcc, NewHeightAcc).
create_board(Width, Height, Board, Acc, WidthAcc, HeightAcc) :-
	WidthAcc > 1,
	HeightAcc > 0,
	color(Color),
	append([block(Color, WidthAcc, HeightAcc)], Acc, NewAcc),
	NewWidthAcc is WidthAcc - 1,
	create_board(Width, Height, Board, NewAcc, NewWidthAcc, HeightAcc).
	
find_same_horizontal(Board, Same) :-


game(Board, Width, Height) :-
	create_board(Width, Height, Board),
	find_same_horizontal(Board, []),
	find_same_vertical(Board, []).
	
query(create_board(4, 4, Board)).
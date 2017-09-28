pack([],[]).
pack([X|Xs],[Z|Zs]) :- transfer(X,Xs,Ys,Z), pack(Ys,Zs).

transfer(X,[],[],[X]).
transfer(X,[Y|Ys],[Y|Ys],[X]) :- X \= Y.
transfer(X,[X|Xs],Ys,[X|Zs]) :- transfer(X,Xs,Ys,Zs).

transform([], Result, Result, Index).
transform([[X|Xs]|Ys], Result, Acc, Index) :- 
	length([X|Xs],N),
	check_length(N, X, Index, Acc, NewAcc),
	NewIndex is Index + N,
	transform(Ys, Result, NewAcc, NewIndex).
	
check_length(Length, X, Index, Acc, NewAcc) :-
	Length >= 3,
	append(Acc, [same(X, Index, Length)], NewAcc).
	
check_length(Length, X, Index, Acc, NewAcc) :-
	Length < 3,
	NewAcc = Acc.
	
append([],L,L). 
append([H|T],L2,[H|L3])  :-  append(T,L2,L3).

check_for_3_or_more(L1,L2) :- pack(L1,L), transform(L, L2, [], 0).

check_horizontal(Board, Result) :-
	check_horizontal(Board, Result, []).
check_horizontal(board([], _, _), Result, Result).
check_horizontal(board(List, Width, _), Result, Acc) :-
	get_row_from_list(List, Width, Row, Remaining),
	check_for_3_or_more(Row, RowResult),
	append(Acc, [RowResult], NewAcc),
	check_horizontal(board(Remaining, Width, _), Result, NewAcc).

check_vertical(Board, Result) :-
	check_vertical(Board, Result, []).
check_vertical(board([], _, _), Result, Result).
check_vertical(board(List, _, Height), Result, Acc) :-
	get_row_from_list(List, Height, Column, Remaining),
	check_for_3_or_more(Column, ColumnResult),
	append(Acc, [ColumnResult], NewAcc),
	check_vertical(board(Remaining, _, Height), Result, NewAcc).


	
get_row_from_list(List, Width, Result, Remaining) :-
	get_row_from_list(List, Width, Result, [], Remaining).	
get_row_from_list(Remaining, 0, Result, Result, Remaining).	
get_row_from_list([H|T], Width, Result, Acc, Remaining) :-
	append(Acc, [H], NewAcc),
	NewWidth is Width - 1,
	get_row_from_list(T, NewWidth, Result, NewAcc, Remaining).
	
get_column_from_list(List, Width, Height, Result, Remaining) :-
	get_column_from_list(List, Width, Height, Result, [], Remaining).	
get_column_from_list(Remaining, Width, 0, Result, Result, Remaining).	
get_column_from_list([H|T], Width, Height, Result, Acc, Remaining) :-
	append(Acc, [H], NewAcc),
	NewHeight is Height - 1,
	get_column_from_list(T, Width, NewHeight, Result, NewAcc, Remaining).

	
%query(check_for_3_or_more([r,g,b,b,b,b,r,r,r,g,b],List)).
%query(get_row_from_list([r,g,b,b,b,b,r,r,r,g,b], 5, Result, Remaining)).
query(check_horizontal(board([r,r,r,g,g,g,y,y,r,b,b,b,y,y,r,b,b,y,r,g,b,r,g,b], 6, 4), Result)).
query(check_vertical(board([r,r,r,g,g,g,y,y,r,b,b,b,y,y,r,b,b,y,r,g,b,r,g,b], 6, 4), Result)).
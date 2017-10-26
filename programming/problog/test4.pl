insert_at_end(X,[],[X]).
insert_at_end(X,[H|T],[H|Z]) :- insert_at_end(X,T,Z).

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
	
	
%find_same_in_list(List, Same) :-
%	find_same_in_list(List, Same, [], []).
%find_same_in_list([], Same, Acc, TotalAcc) :-
%	length(Acc, Length),
%	Length > 2,
%	append(Acc, TotalAcc, Same).
%find_same_in_list([], Same, Acc, TotalAcc) :-
%	length(Acc, Length),
%	Length =< 2,
%	Same = TotalAcc.
%find_same_in_list([Block|Tail], Same, [], TotalAcc) :-
%	find_same_in_list(Tail, Same, [Block], TotalAcc).
%find_same_in_list([block(Color1, X1, Y1)|Tail], Same, [block(Color2, X2, Y2)|TailAcc], TotalAcc) :-
%	color(Color1),
%	Color1 == Color2,
%	NewAcc = [block(Color1, X1, Y1),block(Color2, X2, Y2)|TailAcc],
%	find_same_in_list(Tail, Same, NewAcc, TotalAcc).
%find_same_in_list([block(Color1, X1, Y1)|Tail], Same, [block(Color2, X2, Y2)|TailAcc], Acc) :-
%	Color1 \= Color2,
%	length([block(Color2, X2, Y2)|TailAcc], Length),
%	Length > 2,
%	append([block(Color2, X2, Y2)|TailAcc], Acc, NewAcc),
%	find_same_in_list(Tail, Same, [block(Color1, X1, Y1)], NewAcc).
%find_same_in_list([block(Color1, X1, Y1)|Tail], Same, [block(Color2, X2, Y2)|TailAcc], Acc) :-
%	Color1 \= Color2,
%	length([block(Color2, X2, Y2)|TailAcc], Length),
%	Length =< 2,
%	find_same_in_list(Tail, Same, [block(Color1, X1, Y1)], Acc).
% een bord bestaat uit een grid van 10 op 10. Deze zijn gevuld met kleuren. op elke kleur kan gedrukt worden
% om deze te laten veranderen in een andere random kleur.
% Het bord wordt voorgesteld met een lijst.
% vb. bord van 3 op 3: [red, blue, yellow, red, green, yellow, green, green, yellow]
% dit geeft dan het bord:
%
% |R B Y|
% |R G Y|
% |G G Y|
% 

PR::color(C,r,PR,PG,PB,PY); PG::color(C,g,PR,PG,PB,PY); PB::color(C,b,PR,PG,PB,PY); PY::color(C,y,PR,PG,PB,PY).

% probabilities in which color the color changes when certain color is pressed
red_color(C,S) :- color(C, S, 0, 1/3, 1/3, 1/3).
green_color(C,S) :- color(C, S, 1/3, 0, 1/3, 1/3).
blue_color(C,S) :- color(C, S, 1/3, 1/3, 0, 1/3).
yellow_color(C,S) :- color(C, S, 1/3, 1/3, 1/3, 0).

% this predicate will check if there are 3 or more of the same color next to each other horizontally.
% the result will give the list: [[same(Color, X1, length), ...], ...]
% example for a 7 on 7 grid board: [[same(red, 0, 3), same(green, 3, 4)], [], [same(blue, 1, 5)], [], [], [], [same(yellow, 2, 5)]]
% A board of the following construction can deliver this result:
%
% R R R G G G G
% R G B R G B R
% G B B B B B G
% R G B R G B R
% G B R G B R G
% B R G B R G B
% R G Y Y Y Y Y
%
check_horizontal(board([], _, _), _, Result, Result).
check_horizontal(board([Color|Tail],Width, _), Index, Result, Acc) :-
	
	check_horizontal(board(Tail, Width, _), Index, Result, Acc).
	
check_horizontal_row([], _, _, Result, Result)
check_horizontal_row([Color|Tail], AccColor, Length, Result, Acc) :-
	

% query(red_color(r1,_)).
% query(green_color(g1,_)).
% query(blue_color(b1,_)).
% query(yellow_color(y1,_)).
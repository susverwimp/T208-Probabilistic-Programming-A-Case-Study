width(3).
height(3).

1/5::uniform_color(X, Y, red).
1/4::uniform_color(X, Y, green).
1/3::uniform_color(X, Y, blue).
1/2::uniform_color(X, Y, yellow).

color(red).
color(green).
color(blue).
color(yellow).

position(X, Y) :-
	width(Width),
	height(Height),
	between(1,Width,X),
	between(1,Height,Y).
	
block(X, Y, red) :- 
	position(X, Y),
	uniform_color(X, Y, red).

block(X, Y, green) :- 
	position(X, Y),
	\+ uniform_color(X, Y, red),
	uniform_color(X, Y, green).
	
block(X, Y, blue) :- 
	position(X, Y),
	\+ uniform_color(X, Y, red),
	\+ uniform_color(X, Y, green),
	uniform_color(X, Y, blue).
	
block(X, Y, yellow) :- 
	position(X, Y),
	\+ uniform_color(X, Y, red),
	\+ uniform_color(X, Y, green),
	\+ uniform_color(X, Y, blue),
	uniform_color(X, Y, yellow).
	
block(X, Y, white) :- 
	position(X, Y),
	\+ uniform_color(X, Y, red),
	\+ uniform_color(X, Y, green),
	\+ uniform_color(X, Y, blue),
	\+ uniform_color(X, Y, yellow).

	
% ratio_strategy(Color) :-
	% ratio_strategy(Color, 1, 1).
% ratio_strategy(Color, X, Y) :-
	% \+ block(X, Y, _),
	% NewX is X + 1,
	% \+ block(NewX, 1, _).
% ratio_strategy(Color, X, Y) :-
	% block(X,Y,Color),
	% press()
	% NewY is Y + 1,
	% ratio_strategy(Color, X, NewY).
% ratio_strategy(Color, X, Y) :-
	% \+ block(X, Y, _),
	% NewX is X + 1,
	% block(NewX,1,Color),
	% ratio_strategy(Color, NewX, 2).
	
color_ratio(ColorRatio) :-
	color_list(ColorList),
	list_ratio(ColorList, [Red, Green, Blue, Yellow]),
	ColorRatio = [[red, Red], [green, Green], [blue, Blue], [yellow, Yellow]].
	
color_list(ColorList) :-
	color_list(ColorList, 1, 1, []).
color_list(ColorList, X, Y, ColorList) :-
	\+ block(X, Y, _),
	NewX is X + 1,
	\+ block(NewX, 1, _).
color_list(ColorList, X, Y, ColorListAcc) :-
	block(X,Y,Color),
	NewColorListAcc = [Color|ColorListAcc],
	NewY is Y + 1,
	color_list(ColorList, X, NewY, NewColorListAcc).
color_list(ColorList, X, Y, ColorListAcc) :-
	\+ block(X, Y, _),
	NewX is X + 1,
	block(NewX,1,Color),
	NewColorListAcc = [Color|ColorListAcc],
	color_list(ColorList, NewX, 2, NewColorListAcc).

list_ratio(List, ListRatio) :-
	list_ratio(List, ListRatio, 0, 0, 0, 0).
list_ratio([], [RedAcc, GreenAcc, BlueAcc, YellowAcc], RedAcc, GreenAcc, BlueAcc, YellowAcc).
list_ratio([Color|Tail], ListRatio, RedAcc, GreenAcc, BlueAcc, YellowAcc) :-
	Color = red,
	NewRedAcc is RedAcc + 1,
	list_ratio(Tail, ListRatio, NewRedAcc, GreenAcc, BlueAcc, YellowAcc).
list_ratio([Color|Tail], ListRatio, RedAcc, GreenAcc, BlueAcc, YellowAcc) :-
	Color = green,
	NewGreenAcc is GreenAcc + 1,
	list_ratio(Tail, ListRatio, RedAcc, NewGreenAcc, BlueAcc, YellowAcc).
list_ratio([Color|Tail], ListRatio, RedAcc, GreenAcc, BlueAcc, YellowAcc) :-
	Color = blue,
	NewBlueAcc is BlueAcc + 1,
	list_ratio(Tail, ListRatio, RedAcc, GreenAcc, NewBlueAcc, YellowAcc).
list_ratio([Color|Tail], ListRatio, RedAcc, GreenAcc, BlueAcc, YellowAcc) :-
	Color = yellow,
	NewYellowAcc is YellowAcc + 1,
	list_ratio(Tail, ListRatio, RedAcc, GreenAcc, BlueAcc, NewYellowAcc).
list_ratio([Color|Tail], ListRatio, RedAcc, GreenAcc, BlueAcc, YellowAcc) :-
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
	
evidence(block(1,1,red), true).
evidence(block(1,1,green), false).
evidence(block(1,1,blue), false).
evidence(block(1,1,yellow), false).
evidence(block(1,1,white), false).

evidence(block(1,2,red), false).
evidence(block(1,2,green), true).
evidence(block(1,2,blue), false).
evidence(block(1,2,yellow), false).
evidence(block(1,2,white), false).

evidence(block(1,3,red), false).
evidence(block(1,3,green), false).
evidence(block(1,3,blue), true).
evidence(block(1,3,yellow), false).
evidence(block(1,3,white), false).

evidence(block(2,1,red), false).
evidence(block(2,1,green), false).
evidence(block(2,1,blue), true).
evidence(block(2,1,yellow), false).
evidence(block(2,1,white), false).

evidence(block(2,2,red), false).
evidence(block(2,2,green), true).
evidence(block(2,2,blue), false).
evidence(block(2,2,yellow), false).
evidence(block(2,2,white), false).

evidence(block(2,3,red), false).
evidence(block(2,3,green), false).
evidence(block(2,3,blue), false).
evidence(block(2,3,yellow), true).
evidence(block(2,3,white), false).

evidence(block(3,1,red), true).
evidence(block(3,1,green), false).
evidence(block(3,1,blue), false).
evidence(block(3,1,yellow), false).
evidence(block(3,1,white), false).

evidence(block(3,2,red), false).
evidence(block(3,2,green), true).
evidence(block(3,2,blue), false).
evidence(block(3,2,yellow), false).
evidence(block(3,2,white), false).

evidence(block(3,3,red), false).
evidence(block(3,3,green), false).
evidence(block(3,3,blue), true).
evidence(block(3,3,yellow), false).
evidence(block(3,3,white), false).

% query(color_ratio_strategy(T)).

query(block(1,1,Color)).
query(block(1,2,Color)).
query(block(1,3,Color)).
query(block(2,1,Color)).
query(block(2,2,Color)).
query(block(2,3,Color)).
query(block(3,1,Color)).
query(block(3,2,Color)).
query(block(3,3,Color)).

query(color_ratio(Ratio)).
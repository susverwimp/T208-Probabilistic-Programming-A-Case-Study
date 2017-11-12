width(3).
height(3).

1/5::uniform_color(X, Y, red).
1/4::uniform_color(X, Y, green).
1/3::uniform_color(X, Y, blue).
1/2::uniform_color(X, Y, yellow).

P::press(X, Y, Color) :-
	block(X, Y, Color),
	color(Color),
	color_ratio([[_,RedCount],[_,GreenCount],[_,BlueCount],[_,YellowCount]]),
	P is 1 / (RedCount + GreenCount + BlueCount + YellowCount).
	

color(red).
color(green).
color(blue).
color(yellow).

position(X, Y) :-
	width(Width),
	height(Height),
	between(1,Width,X),
	between(1,Height,Y).
	
block(X, 1, red) :-
	position(X, 1),
	uniform_color(X, 1, red).
block(X, Y, red) :- 
	Y > 1,
	position(X, Y),
	uniform_color(X, Y, red),
	Y2 is Y - 1,
	block(X, Y2, Color),
	color(Color).

block(X, 1, green) :- 
	position(X, 1),
	\+ uniform_color(X, 1, red),
	uniform_color(X, 1, green).
block(X, Y, green) :- 
	Y > 1,
	position(X, Y),
	\+ uniform_color(X, Y, red),
	uniform_color(X, Y, green),
	Y2 is Y - 1,
	block(X, Y2, Color),
	color(Color).

block(X, 1, blue) :- 
	position(X, 1), 
	\+ uniform_color(X, 1, red), 
	\+ uniform_color(X, 1, green), 
	uniform_color(X, 1, blue).	
block(X, Y, blue) :-
	Y > 1,
	position(X, Y), 
	\+ uniform_color(X, Y, red), 
	\+ uniform_color(X, Y, green), 
	uniform_color(X, Y, blue),
	Y2 is Y - 1,
	block(X, Y2, Color),
	color(Color).

block(X, 1, yellow) :- 
	position(X, 1), 
	\+ uniform_color(X, 1, red), 
	\+ uniform_color(X, 1, green), 
	\+ uniform_color(X, 1, blue), 
	uniform_color(X, 1, yellow).
block(X, Y, yellow) :- 
	Y > 1,
	position(X, Y), 
	\+ uniform_color(X, Y, red), 
	\+ uniform_color(X, Y, green), 
	\+ uniform_color(X, Y, blue), 
	uniform_color(X, Y, yellow),
	Y2 is Y - 1,
	block(X, Y2, Color),
	color(Color).

block(X, Y, white) :- 
	height(Y),
	position(X, Y), 
	\+ uniform_color(X, Y, red), 
	\+ uniform_color(X, Y, green), 
	\+ uniform_color(X, Y, blue), 
	\+ uniform_color(X, Y, yellow).
block(X, Y, white) :- 
	height(Height),
	Y < Height,
	position(X, Y), 
	\+ uniform_color(X, Y, red), 
	\+ uniform_color(X, Y, green), 
	\+ uniform_color(X, Y, blue), 
	\+ uniform_color(X, Y, yellow),
	Y2 is Y + 1,
	block(X, Y2, white).
	
color_ratio(ColorRatio) :-
	color_list(ColorList),
	list_ratio(ColorList, [Red, Green, Blue, Yellow]),
	ColorRatio = [[red, Red], [green, Green], [blue, Blue], [yellow, Yellow]].
	
color_list(ColorList) :-
	color_list(ColorList, 1, 1, []).
color_list(ColorList, X, Y, ColorList) :-
	\+ position(X, Y),
	NewX is X + 1,
	\+ position(NewX, 1).
color_list(ColorList, X, Y, ColorListAcc) :-
	position(X, Y),
	block(X,Y,Color),
	NewColorListAcc = [Color|ColorListAcc],
	NewY is Y + 1,
	color_list(ColorList, X, NewY, NewColorListAcc).
color_list(ColorList, X, Y, ColorListAcc) :-
	\+ position(X, Y),
	NewX is X + 1,
	position(NewX, 1),
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

test(T) :-
	color_ratio(ColorRatio),
	min_ratio(ColorRatio, [T,_]).
	
color_ratio_strategy :-
	color_ratio(ColorRatio),
	min_ratio(ColorRatio, [Color,_]),
	color_ratio_strategy(Color).
color_ratio_strategy(red) :-
	press(X, Y, red),
	\+ press(X, Y, green),
	\+ press(X, Y, blue),
	\+ press(X, Y, yellow).
color_ratio_strategy(green) :-
	\+ press(X, Y, red),
	press(X, Y, green),
	\+ press(X, Y, blue),
	\+ press(X, Y, yellow).
color_ratio_strategy(blue) :-
	\+ press(X, Y, red),
	\+ press(X, Y, green),
	press(X, Y, blue),
	\+ press(X, Y, yellow).
color_ratio_strategy(yellow) :-
	\+ press(X, Y, red),
	\+ press(X, Y, green),
	\+ press(X, Y, blue),
	press(X, Y, yellow).
	
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

evidence(color_ratio_strategy, true).

% query(block(1,1,Color)).
% query(block(1,2,Color)).
% query(block(1,3,Color)).
% query(block(2,1,Color)).
% query(block(2,2,Color)).
% query(block(2,3,Color)).
% query(block(3,1,Color)).
% query(block(3,2,Color)).
% query(block(3,3,Color)).

% query(color_ratio(List)).

% query(press(1,1,Color)).
% query(press(1,2,Color)).
% query(press(1,3,Color)).
% query(press(2,1,Color)).
% query(press(2,2,Color)).
% query(press(2,3,Color)).
% query(press(3,1,Color)).
% query(press(3,2,Color)).
% query(press(3,3,Color)).

query(test(T)).
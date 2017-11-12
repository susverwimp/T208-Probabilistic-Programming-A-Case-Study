1/5::block(X, Y, red); 1/5::block(X, Y, green); 1/5::block(X, Y, blue); 1/5::block(X, Y, yellow); 1/5::block(X, Y, white).

1/3::change(red, blue); 1/3::change(red, green); 1/3::change(red, yellow).
1/3::change(blue, red); 1/3::change(blue, green); 1/3::change(blue, yellow).
1/3::change(green, red); 1/3::change(green, blue); 1/3::change(green, yellow).
1/3::change(yellow, red); 1/3::change(yellow, blue); 1/3::change(yellow, green).


ProbPress::press(X, Y) :-
	location(X, Y),
	color_ratio([[_,Red], [_,Green], [_,Blue], [_,Yellow]]),
	Sum is Red + Green + Blue + Yellow,
	ProbPress is 1/Sum.

color(red).
color(green).
color(blue).
color(yellow).

color_below(X, Y) :-
	Y2 is Y - 1,
	\+ location(X, Y2).
color_below(X, Y) :-
	Y2 is Y - 1,
	location(X, Y2),
	\+ block(X, Y2, white),
	color_below(X, Y2).

red(X, Y) :-
	\+ location(X, Y).
red(X, Y) :-
	location(X, Y),
	block(X, Y, red),
	\+ block(X, Y, green),
	\+ block(X, Y, blue),
	\+ block(X, Y, yellow),
	\+ block(X, Y, white),
	color_below(X, Y).

green(X, Y) :-
	\+ location(X, Y).
green(X, Y) :-
	location(X, Y),
	\+ block(X, Y, red),
	block(X, Y, green),
	\+ block(X, Y, blue),
	\+ block(X, Y, yellow),
	\+ block(X, Y, white),
	color_below(X, Y).
	
blue(X, Y) :-
	\+ location(X, Y).
blue(X, Y) :-
	location(X, Y),
	\+ block(X, Y, red),
	\+ block(X, Y, green),
	block(X, Y, blue),
	\+ block(X, Y, yellow),
	\+ block(X, Y, white),
	color_below(X, Y).
	
yellow(X, Y) :-
	\+ location(X, Y).
yellow(X, Y) :-
	location(X, Y),
	\+ block(X, Y, red),
	\+ block(X, Y, green),
	\+ block(X, Y, blue),
	block(X, Y, yellow),
	\+ block(X, Y, white),
	color_below(X, Y).
	
white(X, Y) :-
	\+ location(X, Y).
white(X, Y) :-
	location(X, Y),
	\+ block(X, Y, red),
	\+ block(X, Y, green),
	\+ block(X, Y, blue),
	\+ block(X, Y, yellow),
	block(X, Y, white),
	Y2 is Y + 1,
	white(X, Y2).


location(X, Y) :-
	between(1,3,X),
	between(1,3,Y).

right_same_color(X, Y) :-
	X2 is X + 1,
	location(X, Y),
	location(X2, Y),
	block(X, Y, Color),
	block(X2, Y, Color),
	color(Color).
	
left_same_color(X, Y) :-
	X2 is X - 1,
	location(X, Y),
	location(X2, Y),
	block(X, Y, Color),
	block(X2, Y, Color),
	color(Color).
	
up_same_color(X, Y) :-
	Y2 is Y + 1,
	location(X, Y),
	location(X, Y2),
	block(X, Y, Color),
	block(X, Y2, Color),
	color(Color).
	
down_same_color(X, Y) :-
	Y2 is Y - 1,
	location(X, Y),
	location(X, Y2),
	block(X, Y, Color),
	block(X, Y2, Color),
	color(Color).
	
left_and_right_same_color(X, Y) :-
	X2 is X - 1,
	X3 is Y + 1,
	location(X2, Y),
	location(X3, Y),
	block(X2, Y, Color),
	block(X3, Y, Color),
	color(Color).
	
down_and_up_same_color(X, Y) :-
	Y2 is Y - 1,
	Y3 is Y + 1,
	location(X, Y2),
	location(X, Y3),
	block(X, Y2, Color),
	block(X, Y3, Color),
	color(Color).
	
possible_score(X, Y, 3) :-
	Xright is X + 1,
	Xleft is X - 1,
	Yup is Y + 1,
	Ydown is Y - 1,
	right_same_color(Xright, Y),
	\+ right_same_color(X, Y),
	\+ left_same_color(Xleft, Y),
	\+ up_same_color(X, Yup),
	\+ down_same_color(X, Ydown),
	\+ left_and_right_same_color(X, Y),
	\+ down_and_up_same_color(X, Y).

possible_score(X, Y, 3) :-
	Xright is X + 1,
	Xleft is X - 1,
	Yup is Y + 1,
	Ydown is Y - 1,
	\+right_same_color(Xright, Y),
	left_same_color(Xleft, Y),
	\+ left_same_color(X, Y),
	\+ up_same_color(X, Yup),
	\+ down_same_color(X, Ydown),
	\+ left_and_right_same_color(X, Y),
	\+ down_and_up_same_color(X, Y).
	
possible_score(X, Y, 3) :-
	Xright is X + 1,
	Xleft is X - 1,
	Yup is Y + 1,
	Ydown is Y - 1,
	\+ right_same_color(Xright, Y),
	\+ left_same_color(Xleft, Y),
	up_same_color(X, Yup),
	\+ up_same_color(X, Y),
	\+ down_same_color(X, Ydown),
	\+ left_and_right_same_color(X, Y),
	\+ down_and_up_same_color(X, Y).
	
possible_score(X, Y, 3) :-
	Xright is X + 1,
	Xleft is X - 1,
	Yup is Y + 1,
	Ydown is Y - 1,
	\+ right_same_color(Xright, Y),
	\+ left_same_color(Xleft, Y),
	\+ up_same_color(X, Yup),
	down_same_color(X, Ydown),
	\+ down_same_color(X, Y),
	\+ left_and_right_same_color(X, Y),
	\+ down_and_up_same_color(X, Y).
	
possible_score(X, Y, 3) :-
	Xright is X + 1,
	Xleft is X - 1,
	Yup is Y + 1,
	Ydown is Y - 1,
	\+ right_same_color(Xright, Y),
	\+ left_same_color(Xleft, Y),
	\+ up_same_color(X, Yup),
	\+ down_same_color(X, Ydown),
	left_and_right_same_color(X, Y),
	\+ right_same_color(X, Y),
	\+ down_and_up_same_color(X, Y).
	
possible_score(X, Y, 3) :-
	Xright is X + 1,
	Xleft is X - 1,
	Yup is Y + 1,
	Ydown is Y - 1,
	\+ right_same_color(Xright, Y),
	\+ left_same_color(Xleft, Y),
	\+ up_same_color(X, Yup),
	\+ down_same_color(X, Ydown),
	\+ left_and_right_same_color(X, Y),
	down_and_up_same_color(X, Y),
	\+ up_same_color(X, Y).

color_ratio(ColorRatio) :-
	color_list(ColorList),
	list_ratio(ColorList, [Red, Green, Blue, Yellow]),
	ColorRatio = [[red, Red], [green, Green], [blue, Blue], [yellow, Yellow]].

color_list(ColorList) :-
	color_list(ColorList, 1, 1, []).
color_list(ColorList, X, Y, ColorList) :-
	\+ location(X, Y),
	NewX is X + 1,
	\+ location(NewX, 1).
color_list(ColorList, X, Y, ColorListAcc) :-
	location(X, Y),
	block(X,Y,Color),
	NewColorListAcc = [Color|ColorListAcc],
	NewY is Y + 1,
	color_list(ColorList, X, NewY, NewColorListAcc).
color_list(ColorList, X, Y, ColorListAcc) :-
	\+ location(X, Y),
	NewX is X + 1,
	location(NewX, 1),
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
	
	
evidence(red(1,1)).
evidence(green(1,2)).
evidence(blue(1,3)).
evidence(blue(2,1)).
evidence(red(2,2)).
evidence(red(2,3)).
evidence(yellow(3,1)).
%evidence(red(3,2)).
%evidence(yellow(3,3)).
%evidence(white(3,2)).

%evidence(right_same_color(1,2)).
%evidence(possible_score(1,2,3)).

/*
query(red(		1,1)).
query(green(	1,1)).
query(blue(		1,1)).
query(yellow(	1,1)).
query(white(	1,1)).
query(red(		1,2)).
query(green(	1,2)).
query(blue(		1,2)).
query(yellow(	1,2)).
query(white(	1,2)).
query(red(		1,3)).
query(green(	1,3)).
query(blue(		1,3)).
query(yellow(	1,3)).
query(white(	1,3)).
query(red(		2,1)).
query(green(	2,1)).
query(blue(		2,1)).
query(yellow(	2,1)).
query(white(	2,1)).
query(red(		2,2)).
query(green(	2,2)).
query(blue(		2,2)).
query(yellow(	2,2)).
query(white(	2,2)).
query(red(		2,3)).
query(green(	2,3)).
query(blue(		2,3)).
query(yellow(	2,3)).
query(white(	2,3)).
query(red(		3,1)).
query(green(	3,1)).
query(blue(		3,1)).
query(yellow(	3,1)).
query(white(	3,1)).

query(red(		3,2)).
query(green(	3,2)).
query(blue(		3,2)).
query(yellow(	3,2)).
query(white(	3,2)).

query(red(		3,3)).
query(green(	3,3)).
query(blue(		3,3)).
query(yellow(	3,3)).
query(white(	3,3)).
*/

query(color_list(List)).

%query(color_ratio(ColorRatio)).

%query(press(X, Y)).
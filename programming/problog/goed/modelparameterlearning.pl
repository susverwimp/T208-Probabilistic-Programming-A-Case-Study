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
	
right_same_color(X, Y) :-
	X2 is X + 1,
	block(X, Y, Color),
	block(X2, Y, Color).
	
left_same_color(X, Y) :-
	X2 is X - 1,
	block(X, Y, Color),
	block(X2, Y, Color).
	
up_same_color(X, Y) :-
	Y2 is Y + 1,
	block(X, Y, Color),
	block(X, Y2, Color).
	
down_same_color(X, Y) :-
	Y2 is Y - 1,
	location(X, Y),
	location(X, Y2),
	block(X, Y, Color),
	block(X, Y2, Color).
	
left_and_right_same_color(X, Y) :-
	X2 is X - 1,
	X3 is Y + 1,
	block(X2, Y, Color),
	block(X3, Y, Color).
	
down_and_up_same_color(X, Y) :-
	Y2 is Y - 1,
	Y3 is Y + 1,
	block(X, Y2, Color),
	block(X, Y3, Color).
	
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

% evidence(block(3,2,red), false).
% evidence(block(3,2,green), true).
% evidence(block(3,2,blue), false).
% evidence(block(3,2,yellow), false).
% evidence(block(3,2,white), false).

% evidence(block(3,3,red), false).
% evidence(block(3,3,green), false).
% evidence(block(3,3,blue), true).
% evidence(block(3,3,yellow), false).
% evidence(block(3,3,white), false).

evidence(up_same_color(3,2)).

query(block(1,1,Color)).
query(block(1,2,Color)).
query(block(1,3,Color)).
query(block(2,1,Color)).
query(block(2,2,Color)).
query(block(2,3,Color)).
query(block(3,1,Color)).
query(block(3,2,Color)).
query(block(3,3,Color)).

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
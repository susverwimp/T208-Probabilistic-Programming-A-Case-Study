position(1,T).
position(2,T).
position(3,T).
position(4,T).

0.125::uniformT(P,c1,T).
0.14285714285714285::uniformT(P,c2,T).
0.16666666666666666::uniformT(P,c3,T).
0.2::uniformT(P,c4,T).
0.25::uniformT(P,c5,T).
0.3333333333333333::uniformT(P,c6,T).
0.5::uniformT(P,c7,T).

0.125::uniformG(P,c1,T).
0.14285714285714285::uniformG(P,c2,T).
0.16666666666666666::uniformG(P,c3,T).
0.2::uniformG(P,c4,T).
0.25::uniformG(P,c5,T).
0.3333333333333333::uniformG(P,c6,T).
0.5::uniformG(P,c7,T).

true_color(P,c1,0) :- position(P,0), uniformT(P,c1,0).
true_color(P,c2,0) :- position(P,0), \+ uniformT(P,c1,0), uniformT(P,c2,0).
true_color(P,c3,0) :- position(P,0), \+ uniformT(P,c1,0), \+ uniformT(P,c2,0), uniformT(P,c3,0).
true_color(P,c4,0) :- position(P,0), \+ uniformT(P,c1,0), \+ uniformT(P,c2,0), \+ uniformT(P,c3,0), uniformT(P,c4,0).
true_color(P,c5,0) :- position(P,0), \+ uniformT(P,c1,0), \+ uniformT(P,c2,0), \+ uniformT(P,c3,0), \+ uniformT(P,c4,0), uniformT(P,c5,0).
true_color(P,c6,0) :- position(P,0), \+ uniformT(P,c1,0), \+ uniformT(P,c2,0), \+ uniformT(P,c3,0), \+ uniformT(P,c4,0), \+ uniformT(P,c5,0), uniformT(P,c6,0).
true_color(P,c7,0) :- position(P,0), \+ uniformT(P,c1,0), \+ uniformT(P,c2,0), \+ uniformT(P,c3,0), \+ uniformT(P,c4,0), \+ uniformT(P,c5,0), \+ uniformT(P,c6,0), uniformT(P,c7,0).
true_color(P,c8,0) :- position(P,0), \+ uniformT(P,c1,0), \+ uniformT(P,c2,0), \+ uniformT(P,c3,0), \+ uniformT(P,c4,0), \+ uniformT(P,c5,0), \+ uniformT(P,c6,0), \+ uniformT(P,c7,0).

true_color(P,c1,TT) :- TT > 0, T is TT - 1, true_color(P,c1,T).
true_color(P,c2,TT) :- TT > 0, T is TT - 1, true_color(P,c2,T).
true_color(P,c3,TT) :- TT > 0, T is TT - 1, true_color(P,c3,T).
true_color(P,c4,TT) :- TT > 0, T is TT - 1, true_color(P,c4,T).
true_color(P,c5,TT) :- TT > 0, T is TT - 1, true_color(P,c5,T).
true_color(P,c6,TT) :- TT > 0, T is TT - 1, true_color(P,c6,T).
true_color(P,c7,TT) :- TT > 0, T is TT - 1, true_color(P,c7,T).
true_color(P,c8,TT) :- TT > 0, T is TT - 1, true_color(P,c8,T).

guess_color(P,c1,T) :- position(P,T), uniformG(P,c1,T).
guess_color(P,c2,T) :- position(P,T), \+ uniformG(P,c1,T), uniformG(P,c2,T).
guess_color(P,c3,T) :- position(P,T), \+ uniformG(P,c1,T), \+ uniformG(P,c2,T), uniformG(P,c3,T).
guess_color(P,c4,T) :- position(P,T), \+ uniformG(P,c1,T), \+ uniformG(P,c2,T), \+ uniformG(P,c3,T), uniformG(P,c4,T).
guess_color(P,c5,T) :- position(P,T), \+ uniformG(P,c1,T), \+ uniformG(P,c2,T), \+ uniformG(P,c3,T), \+ uniformG(P,c4,T), uniformG(P,c5,T).
guess_color(P,c6,T) :- position(P,T), \+ uniformG(P,c1,T), \+ uniformG(P,c2,T), \+ uniformG(P,c3,T), \+ uniformG(P,c4,T), \+ uniformG(P,c5,T), uniformG(P,c6,T).
guess_color(P,c7,T) :- position(P,T), \+ uniformG(P,c1,T), \+ uniformG(P,c2,T), \+ uniformG(P,c3,T), \+ uniformG(P,c4,T), \+ uniformG(P,c5,T), \+ uniformG(P,c6,T), uniformG(P,c7,T).
guess_color(P,c8,T) :- position(P,T), \+ uniformG(P,c1,T), \+ uniformG(P,c2,T), \+ uniformG(P,c3,T), \+ uniformG(P,c4,T), \+ uniformG(P,c5,T), \+ uniformG(P,c6,T), \+ uniformG(P,c7,T).

first_match(1,1,T).
first_match(1,2,T) :- \+ match(1,1,T).
first_match(1,3,T) :- \+ match(1,1,T), \+ match(1,2,T).
first_match(1,4,T) :- \+ match(1,1,T), \+ match(1,2,T), \+ match(1,3,T).
first_match(2,1,T) :- \+ match(1,1,T).
first_match(2,2,T) :- \+ match(1,2,T), \+ match(2,1,T).
first_match(2,3,T) :- \+ match(1,3,T), \+ match(2,1,T), \+ match(2,2,T).
first_match(2,4,T) :- \+ match(1,4,T), \+ match(2,1,T), \+ match(2,2,T), \+ match(2,3,T).
first_match(3,1,T) :- \+ match(1,1,T), \+ match(2,1,T).
first_match(3,2,T) :- \+ match(1,2,T), \+ match(2,2,T), \+ match(3,1,T).
first_match(3,3,T) :- \+ match(1,3,T), \+ match(2,3,T), \+ match(3,1,T), \+ match(3,2,T).
first_match(3,4,T) :- \+ match(1,4,T), \+ match(2,4,T), \+ match(3,1,T), \+ match(3,2,T), \+ match(3,3,T).
first_match(4,1,T) :- \+ match(1,1,T), \+ match(2,1,T), \+ match(3,1,T).
first_match(4,2,T) :- \+ match(1,2,T), \+ match(2,2,T), \+ match(3,2,T), \+ match(4,1,T).
first_match(4,3,T) :- \+ match(1,3,T), \+ match(2,3,T), \+ match(3,3,T), \+ match(4,1,T), \+ match(4,2,T).
first_match(4,4,T) :- \+ match(1,4,T), \+ match(2,4,T), \+ match(3,4,T), \+ match(4,1,T), \+ match(4,2,T), \+ match(4,3,T).

makeswhite(1,1,T) :- position(P,T), match(1,P,T).
makeswhite(1,2,T) :- position(P,T), match(2,P,T), \+ makeswhite(1,1,T).
makeswhite(1,3,T) :- position(P,T), match(3,P,T), \+ makeswhite(1,1,T), \+ makeswhite(1,2,T).
makeswhite(1,4,T) :- position(P,T), match(4,P,T), \+ makeswhite(1,1,T), \+ makeswhite(1,2,T), \+ makeswhite(1,3,T).
makeswhite(2,1,T) :- position(P,T), match(1,P,T), \+ makeswhite(1,1,T).
makeswhite(2,2,T) :- position(P,T), match(2,P,T), \+ makeswhite(1,2,T), \+ makeswhite(2,1,T).
makeswhite(2,3,T) :- position(P,T), match(3,P,T), \+ makeswhite(1,3,T), \+ makeswhite(2,1,T), \+ makeswhite(2,2,T).
makeswhite(2,4,T) :- position(P,T), match(4,P,T), \+ makeswhite(1,4,T), \+ makeswhite(2,1,T), \+ makeswhite(2,2,T), \+ makeswhite(2,3,T).
makeswhite(3,1,T) :- position(P,T), match(1,P,T), \+ makeswhite(1,1,T), \+ makeswhite(2,1,T).
makeswhite(3,2,T) :- position(P,T), match(2,P,T), \+ makeswhite(1,2,T), \+ makeswhite(2,2,T), \+ makeswhite(3,1,T).
makeswhite(3,3,T) :- position(P,T), match(3,P,T), \+ makeswhite(1,3,T), \+ makeswhite(2,3,T), \+ makeswhite(3,1,T), \+ makeswhite(3,2,T).
makeswhite(3,4,T) :- position(P,T), match(4,P,T), \+ makeswhite(1,4,T), \+ makeswhite(2,4,T), \+ makeswhite(3,1,T), \+ makeswhite(3,2,T), \+ makeswhite(3,3,T).
makeswhite(4,1,T) :- position(P,T), match(1,P,T), \+ makeswhite(1,1,T), \+ makeswhite(2,1,T), \+ makeswhite(3,1,T).
makeswhite(4,2,T) :- position(P,T), match(2,P,T), \+ makeswhite(1,2,T), \+ makeswhite(2,2,T), \+ makeswhite(3,2,T), \+ makeswhite(4,1,T).
makeswhite(4,3,T) :- position(P,T), match(3,P,T), \+ makeswhite(1,3,T), \+ makeswhite(2,3,T), \+ makeswhite(3,3,T), \+ makeswhite(4,1,T), \+ makeswhite(4,2,T).
makeswhite(4,4,T) :- position(P,T), match(4,P,T), \+ makeswhite(1,4,T), \+ makeswhite(2,4,T), \+ makeswhite(3,4,T), \+ makeswhite(4,1,T), \+ makeswhite(4,2,T), \+ makeswhite(4,3,T).

makesblack(1,1,T) :- same_color(1,1,T).
makesblack(1,2,T) :- same_color(2,2,T), \+ makesblack(1,1,T).
makesblack(1,3,T) :- same_color(3,3,T), \+ makesblack(1,1,T), \+ makesblack(1,2,T).
makesblack(1,4,T) :- same_color(4,4,T), \+ makesblack(1,1,T), \+ makesblack(1,2,T), \+ makesblack(1,3,T).
makesblack(2,1,T) :- same_color(1,1,T), \+ makesblack(1,1,T).
makesblack(2,2,T) :- same_color(2,2,T), \+ makesblack(1,2,T), \+ makesblack(2,1,T).
makesblack(2,3,T) :- same_color(3,3,T), \+ makesblack(1,3,T), \+ makesblack(2,1,T), \+ makesblack(2,2,T).
makesblack(2,4,T) :- same_color(4,4,T), \+ makesblack(1,4,T), \+ makesblack(2,1,T), \+ makesblack(2,2,T), \+ makesblack(2,3,T).
makesblack(3,1,T) :- same_color(1,1,T), \+ makesblack(1,1,T), \+ makesblack(2,1,T).
makesblack(3,2,T) :- same_color(2,2,T), \+ makesblack(1,2,T), \+ makesblack(2,2,T), \+ makesblack(3,1,T).
makesblack(3,3,T) :- same_color(3,3,T), \+ makesblack(1,3,T), \+ makesblack(2,3,T), \+ makesblack(3,1,T), \+ makesblack(3,2,T).
makesblack(3,4,T) :- same_color(4,4,T), \+ makesblack(1,4,T), \+ makesblack(2,4,T), \+ makesblack(3,1,T), \+ makesblack(3,2,T), \+ makesblack(3,3,T).
makesblack(4,1,T) :- same_color(1,1,T), \+ makesblack(1,1,T), \+ makesblack(2,1,T), \+ makesblack(3,1,T).
makesblack(4,2,T) :- same_color(2,2,T), \+ makesblack(1,2,T), \+ makesblack(2,2,T), \+ makesblack(3,2,T), \+ makesblack(4,1,T).
makesblack(4,3,T) :- same_color(3,3,T), \+ makesblack(1,3,T), \+ makesblack(2,3,T), \+ makesblack(3,3,T), \+ makesblack(4,1,T), \+ makesblack(4,2,T).
makesblack(4,4,T) :- same_color(4,4,T), \+ makesblack(1,4,T), \+ makesblack(2,4,T), \+ makesblack(3,4,T), \+ makesblack(4,1,T), \+ makesblack(4,2,T), \+ makesblack(4,3,T).

same_color(P,PP,T) :- true_color(PP,C,T), guess_color(P,C,T).
match(P,PP,T) :- same_color(P,PP,T), first_match(P,PP,T).
white(P,T) :- position(P,T), position(PP,T), P =< PP, makeswhite(P,PP,T).
black(P,T) :- position(P,T), position(PP,T), P =< PP, makesblack(P,PP,T).

evidence(guess_color(1,c1,0),false).
evidence(guess_color(1,c2,0),false).
evidence(guess_color(1,c3,0),false).
evidence(guess_color(1,c4,0),false).
evidence(guess_color(1,c5,0),true).
evidence(guess_color(1,c6,0),false).
evidence(guess_color(1,c7,0),false).
evidence(guess_color(1,c8,0),false).
evidence(guess_color(2,c1,0),false).
evidence(guess_color(2,c2,0),false).
evidence(guess_color(2,c3,0),false).
evidence(guess_color(2,c4,0),false).
evidence(guess_color(2,c5,0),true).
evidence(guess_color(2,c6,0),false).
evidence(guess_color(2,c7,0),false).
evidence(guess_color(2,c8,0),false).
evidence(guess_color(3,c1,0),true).
evidence(guess_color(3,c2,0),false).
evidence(guess_color(3,c3,0),false).
evidence(guess_color(3,c4,0),false).
evidence(guess_color(3,c5,0),false).
evidence(guess_color(3,c6,0),false).
evidence(guess_color(3,c7,0),false).
evidence(guess_color(3,c8,0),false).
evidence(guess_color(4,c1,0),false).
evidence(guess_color(4,c2,0),false).
evidence(guess_color(4,c3,0),false).
evidence(guess_color(4,c4,0),false).
evidence(guess_color(4,c5,0),false).
evidence(guess_color(4,c6,0),false).
evidence(guess_color(4,c7,0),true).
evidence(guess_color(4,c8,0),false).
evidence(white(1,0),true).
evidence(white(2,0),false).
evidence(white(3,0),false).
evidence(white(4,0),false).
evidence(black(1,0),true).
evidence(black(2,0),false).
evidence(black(3,0),false).
evidence(black(4,0),false).
evidence(guess_color(1,c1,1),false).
evidence(guess_color(1,c2,1),false).
evidence(guess_color(1,c3,1),false).
evidence(guess_color(1,c4,1),false).
evidence(guess_color(1,c5,1),false).
evidence(guess_color(1,c6,1),true).
evidence(guess_color(1,c7,1),false).
evidence(guess_color(1,c8,1),false).
evidence(guess_color(2,c1,1),false).
evidence(guess_color(2,c2,1),false).
evidence(guess_color(2,c3,1),true).
evidence(guess_color(2,c4,1),false).
evidence(guess_color(2,c5,1),false).
evidence(guess_color(2,c6,1),false).
evidence(guess_color(2,c7,1),false).
evidence(guess_color(2,c8,1),false).
evidence(guess_color(3,c1,1),false).
evidence(guess_color(3,c2,1),true).
evidence(guess_color(3,c3,1),false).
evidence(guess_color(3,c4,1),false).
evidence(guess_color(3,c5,1),false).
evidence(guess_color(3,c6,1),false).
evidence(guess_color(3,c7,1),false).
evidence(guess_color(3,c8,1),false).
evidence(guess_color(4,c1,1),false).
evidence(guess_color(4,c2,1),true).
evidence(guess_color(4,c3,1),false).
evidence(guess_color(4,c4,1),false).
evidence(guess_color(4,c5,1),false).
evidence(guess_color(4,c6,1),false).
evidence(guess_color(4,c7,1),false).
evidence(guess_color(4,c8,1),false).
evidence(white(1,1),true).
evidence(white(2,1),false).
evidence(white(3,1),false).
evidence(white(4,1),false).
evidence(black(1,1),true).
evidence(black(2,1),false).
evidence(black(3,1),false).
evidence(black(4,1),false).
evidence(guess_color(1,c1,2),false).
evidence(guess_color(1,c2,2),false).
evidence(guess_color(1,c3,2),true).
evidence(guess_color(1,c4,2),false).
evidence(guess_color(1,c5,2),false).
evidence(guess_color(1,c6,2),false).
evidence(guess_color(1,c7,2),false).
evidence(guess_color(1,c8,2),false).
evidence(guess_color(2,c1,2),false).
evidence(guess_color(2,c2,2),false).
evidence(guess_color(2,c3,2),false).
evidence(guess_color(2,c4,2),false).
evidence(guess_color(2,c5,2),true).
evidence(guess_color(2,c6,2),false).
evidence(guess_color(2,c7,2),false).
evidence(guess_color(2,c8,2),false).
evidence(guess_color(3,c1,2),false).
evidence(guess_color(3,c2,2),false).
evidence(guess_color(3,c3,2),false).
evidence(guess_color(3,c4,2),false).
evidence(guess_color(3,c5,2),false).
evidence(guess_color(3,c6,2),false).
evidence(guess_color(3,c7,2),true).
evidence(guess_color(3,c8,2),false).
evidence(guess_color(4,c1,2),false).
evidence(guess_color(4,c2,2),true).
evidence(guess_color(4,c3,2),false).
evidence(guess_color(4,c4,2),false).
evidence(guess_color(4,c5,2),false).
evidence(guess_color(4,c6,2),false).
evidence(guess_color(4,c7,2),false).
evidence(guess_color(4,c8,2),false).
evidence(white(1,2),true).
evidence(white(2,2),true).
evidence(white(3,2),false).
evidence(white(4,2),false).
evidence(black(1,2),false).
evidence(black(2,2),false).
evidence(black(3,2),false).
evidence(black(4,2),false).
evidence(guess_color(1,c1,3),true).
evidence(guess_color(1,c2,3),false).
evidence(guess_color(1,c3,3),false).
evidence(guess_color(1,c4,3),false).
evidence(guess_color(1,c5,3),false).
evidence(guess_color(1,c6,3),false).
evidence(guess_color(1,c7,3),false).
evidence(guess_color(1,c8,3),false).
evidence(guess_color(2,c1,3),false).
evidence(guess_color(2,c2,3),true).
evidence(guess_color(2,c3,3),false).
evidence(guess_color(2,c4,3),false).
evidence(guess_color(2,c5,3),false).
evidence(guess_color(2,c6,3),false).
evidence(guess_color(2,c7,3),false).
evidence(guess_color(2,c8,3),false).
evidence(guess_color(3,c1,3),false).
evidence(guess_color(3,c2,3),false).
evidence(guess_color(3,c3,3),true).
evidence(guess_color(3,c4,3),false).
evidence(guess_color(3,c5,3),false).
evidence(guess_color(3,c6,3),false).
evidence(guess_color(3,c7,3),false).
evidence(guess_color(3,c8,3),false).
evidence(guess_color(4,c1,3),false).
evidence(guess_color(4,c2,3),false).
evidence(guess_color(4,c3,3),false).
evidence(guess_color(4,c4,3),true).
evidence(guess_color(4,c5,3),false).
evidence(guess_color(4,c6,3),false).
evidence(guess_color(4,c7,3),false).
evidence(guess_color(4,c8,3),false).
evidence(white(1,3),true).
evidence(white(2,3),false).
evidence(white(3,3),false).
evidence(white(4,3),false).
evidence(black(1,3),true).
evidence(black(2,3),false).
evidence(black(3,3),false).
evidence(black(4,3),false).
evidence(guess_color(1,c1,4),false).
evidence(guess_color(1,c2,4),false).
evidence(guess_color(1,c3,4),false).
evidence(guess_color(1,c4,4),false).
evidence(guess_color(1,c5,4),false).
evidence(guess_color(1,c6,4),false).
evidence(guess_color(1,c7,4),false).
evidence(guess_color(1,c8,4),true).
evidence(guess_color(2,c1,4),false).
evidence(guess_color(2,c2,4),false).
evidence(guess_color(2,c3,4),false).
evidence(guess_color(2,c4,4),false).
evidence(guess_color(2,c5,4),false).
evidence(guess_color(2,c6,4),false).
evidence(guess_color(2,c7,4),false).
evidence(guess_color(2,c8,4),true).
evidence(guess_color(3,c1,4),false).
evidence(guess_color(3,c2,4),false).
evidence(guess_color(3,c3,4),true).
evidence(guess_color(3,c4,4),false).
evidence(guess_color(3,c5,4),false).
evidence(guess_color(3,c6,4),false).
evidence(guess_color(3,c7,4),false).
evidence(guess_color(3,c8,4),false).
evidence(guess_color(4,c1,4),false).
evidence(guess_color(4,c2,4),false).
evidence(guess_color(4,c3,4),false).
evidence(guess_color(4,c4,4),false).
evidence(guess_color(4,c5,4),true).
evidence(guess_color(4,c6,4),false).
evidence(guess_color(4,c7,4),false).
evidence(guess_color(4,c8,4),false).
evidence(white(1,4),true).
evidence(white(2,4),true).
evidence(white(3,4),false).
evidence(white(4,4),false).
evidence(black(1,4),true).
evidence(black(2,4),false).
evidence(black(3,4),false).
evidence(black(4,4),false).
evidence(guess_color(1,c1,5),false).
evidence(guess_color(1,c2,5),false).
evidence(guess_color(1,c3,5),true).
evidence(guess_color(1,c4,5),false).
evidence(guess_color(1,c5,5),false).
evidence(guess_color(1,c6,5),false).
evidence(guess_color(1,c7,5),false).
evidence(guess_color(1,c8,5),false).
evidence(guess_color(2,c1,5),false).
evidence(guess_color(2,c2,5),false).
evidence(guess_color(2,c3,5),false).
evidence(guess_color(2,c4,5),false).
evidence(guess_color(2,c5,5),true).
evidence(guess_color(2,c6,5),false).
evidence(guess_color(2,c7,5),false).
evidence(guess_color(2,c8,5),false).
evidence(guess_color(3,c1,5),false).
evidence(guess_color(3,c2,5),false).
evidence(guess_color(3,c3,5),false).
evidence(guess_color(3,c4,5),false).
evidence(guess_color(3,c5,5),true).
evidence(guess_color(3,c6,5),false).
evidence(guess_color(3,c7,5),false).
evidence(guess_color(3,c8,5),false).
evidence(guess_color(4,c1,5),true).
evidence(guess_color(4,c2,5),false).
evidence(guess_color(4,c3,5),false).
evidence(guess_color(4,c4,5),false).
evidence(guess_color(4,c5,5),false).
evidence(guess_color(4,c6,5),false).
evidence(guess_color(4,c7,5),false).
evidence(guess_color(4,c8,5),false).
evidence(white(1,5),true).
evidence(white(2,5),true).
evidence(white(3,5),false).
evidence(white(4,5),false).
evidence(black(1,5),false).
evidence(black(2,5),false).
evidence(black(3,5),false).
evidence(black(4,5),false).
evidence(guess_color(1,c1,6),false).
evidence(guess_color(1,c2,6),false).
evidence(guess_color(1,c3,6),false).
evidence(guess_color(1,c4,6),false).
evidence(guess_color(1,c5,6),false).
evidence(guess_color(1,c6,6),false).
evidence(guess_color(1,c7,6),false).
evidence(guess_color(1,c8,6),true).
evidence(guess_color(2,c1,6),false).
evidence(guess_color(2,c2,6),false).
evidence(guess_color(2,c3,6),false).
evidence(guess_color(2,c4,6),false).
evidence(guess_color(2,c5,6),false).
evidence(guess_color(2,c6,6),false).
evidence(guess_color(2,c7,6),false).
evidence(guess_color(2,c8,6),true).
evidence(guess_color(3,c1,6),true).
evidence(guess_color(3,c2,6),false).
evidence(guess_color(3,c3,6),false).
evidence(guess_color(3,c4,6),false).
evidence(guess_color(3,c5,6),false).
evidence(guess_color(3,c6,6),false).
evidence(guess_color(3,c7,6),false).
evidence(guess_color(3,c8,6),false).
evidence(guess_color(4,c1,6),true).
evidence(guess_color(4,c2,6),false).
evidence(guess_color(4,c3,6),false).
evidence(guess_color(4,c4,6),false).
evidence(guess_color(4,c5,6),false).
evidence(guess_color(4,c6,6),false).
evidence(guess_color(4,c7,6),false).
evidence(guess_color(4,c8,6),false).
evidence(white(1,6),false).
evidence(white(2,6),false).
evidence(white(3,6),false).
evidence(white(4,6),false).
evidence(black(1,6),false).
evidence(black(2,6),false).
evidence(black(3,6),false).
evidence(black(4,6),false).
evidence(guess_color(1,c1,7),false).
evidence(guess_color(1,c2,7),false).
evidence(guess_color(1,c3,7),false).
evidence(guess_color(1,c4,7),false).
evidence(guess_color(1,c5,7),false).
evidence(guess_color(1,c6,7),false).
evidence(guess_color(1,c7,7),false).
evidence(guess_color(1,c8,7),true).
evidence(guess_color(2,c1,7),true).
evidence(guess_color(2,c2,7),false).
evidence(guess_color(2,c3,7),false).
evidence(guess_color(2,c4,7),false).
evidence(guess_color(2,c5,7),false).
evidence(guess_color(2,c6,7),false).
evidence(guess_color(2,c7,7),false).
evidence(guess_color(2,c8,7),false).
evidence(guess_color(3,c1,7),true).
evidence(guess_color(3,c2,7),false).
evidence(guess_color(3,c3,7),false).
evidence(guess_color(3,c4,7),false).
evidence(guess_color(3,c5,7),false).
evidence(guess_color(3,c6,7),false).
evidence(guess_color(3,c7,7),false).
evidence(guess_color(3,c8,7),false).
evidence(guess_color(4,c1,7),false).
evidence(guess_color(4,c2,7),false).
evidence(guess_color(4,c3,7),false).
evidence(guess_color(4,c4,7),false).
evidence(guess_color(4,c5,7),true).
evidence(guess_color(4,c6,7),false).
evidence(guess_color(4,c7,7),false).
evidence(guess_color(4,c8,7),false).
evidence(white(1,7),true).
evidence(white(2,7),false).
evidence(white(3,7),false).
evidence(white(4,7),false).
evidence(black(1,7),false).
evidence(black(2,7),false).
evidence(black(3,7),false).
evidence(black(4,7),false).
evidence(guess_color(1,c1,8),false).
evidence(guess_color(1,c2,8),false).
evidence(guess_color(1,c3,8),false).
evidence(guess_color(1,c4,8),false).
evidence(guess_color(1,c5,8),false).
evidence(guess_color(1,c6,8),false).
evidence(guess_color(1,c7,8),false).
evidence(guess_color(1,c8,8),true).
evidence(guess_color(2,c1,8),false).
evidence(guess_color(2,c2,8),true).
evidence(guess_color(2,c3,8),false).
evidence(guess_color(2,c4,8),false).
evidence(guess_color(2,c5,8),false).
evidence(guess_color(2,c6,8),false).
evidence(guess_color(2,c7,8),false).
evidence(guess_color(2,c8,8),false).
evidence(guess_color(3,c1,8),false).
evidence(guess_color(3,c2,8),false).
evidence(guess_color(3,c3,8),false).
evidence(guess_color(3,c4,8),false).
evidence(guess_color(3,c5,8),false).
evidence(guess_color(3,c6,8),true).
evidence(guess_color(3,c7,8),false).
evidence(guess_color(3,c8,8),false).
evidence(guess_color(4,c1,8),false).
evidence(guess_color(4,c2,8),false).
evidence(guess_color(4,c3,8),true).
evidence(guess_color(4,c4,8),false).
evidence(guess_color(4,c5,8),false).
evidence(guess_color(4,c6,8),false).
evidence(guess_color(4,c7,8),false).
evidence(guess_color(4,c8,8),false).
evidence(white(1,8),true).
evidence(white(2,8),false).
evidence(white(3,8),false).
evidence(white(4,8),false).
evidence(black(1,8),true).
evidence(black(2,8),false).
evidence(black(3,8),false).
evidence(black(4,8),false).

query(true_color(1,c1,8)).
query(true_color(1,c2,8)).
query(true_color(1,c3,8)).
query(true_color(1,c4,8)).
query(true_color(1,c5,8)).
query(true_color(1,c6,8)).
query(true_color(1,c7,8)).
query(true_color(1,c8,8)).
query(true_color(2,c1,8)).
query(true_color(2,c2,8)).
query(true_color(2,c3,8)).
query(true_color(2,c4,8)).
query(true_color(2,c5,8)).
query(true_color(2,c6,8)).
query(true_color(2,c7,8)).
query(true_color(2,c8,8)).
query(true_color(3,c1,8)).
query(true_color(3,c2,8)).
query(true_color(3,c3,8)).
query(true_color(3,c4,8)).
query(true_color(3,c5,8)).
query(true_color(3,c6,8)).
query(true_color(3,c7,8)).
query(true_color(3,c8,8)).
query(true_color(4,c1,8)).
query(true_color(4,c2,8)).
query(true_color(4,c3,8)).
query(true_color(4,c4,8)).
query(true_color(4,c5,8)).
query(true_color(4,c6,8)).
query(true_color(4,c7,8)).
query(true_color(4,c8,8)).


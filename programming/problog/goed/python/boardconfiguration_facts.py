from problog.program import PrologString
from problog.engine import DefaultEngine
from problog.logic import Term
from problog.core import ProbLog
from problog import get_evaluatable

p = PrologString("""
:- use_module(library(apply)).
:- use_module(library(lists)).

width(2).
height(2).

pressable_color(red).
pressable_color(green).
pressable_color(blue).
pressable_color(yellow).

block(red,0,2). 	block(red,1,2). 	block(blue,2,2).
block(blue,0,1). 	block(blue,1,1). 	block(yellow,2,1).
block(red,0,0). 	block(red,1,0). 	block(yellow,2,0).
	
%%%%%%%%%%%%%%%%%
% GAME PREDICATES
%%%%%%%%%%%%%%%%%
initial_board(Board) :-
	create_board(Board).

create_board(Board) :-
	create_board(0,Board,[]).
create_board(Y,Board,BoardAcc) :-
	height(Height),
	Y < Height,
	create_row(Y,Row),
	NewBoardAcc = [Row|BoardAcc],
	NewY is Y + 1,
	create_board(NewY,Board,NewBoardAcc).
create_board(Y,Board,BoardAcc) :-
	height(Y),
	create_row(Y,Row),
	Board = [Row|BoardAcc].
	
create_row(Y,Row) :-
	create_row(0,Y,Row,[]).
create_row(X,Y,Row,RowAcc) :-
	width(Width),
	X < Width,
	block(Color,X,Y),
	append(RowAcc,[block(Color,X,Y)],NewRowAcc),
	NewX is X + 1,
	create_row(NewX,Y,Row,NewRowAcc).
create_row(X,Y,Row,RowAcc) :-
	width(X),
	block(Color,X,Y),
	append(RowAcc,[block(Color,X,Y)],Row).
""")

engine = DefaultEngine(label_all=True)

db = engine.prepare(p)

# evidence_block02 = Term('block', Term('blue'),0,2)
# evidence_block12 = Term('block', Term('red'),1,2)
# evidence_block22 = Term('block', Term('red'),2,2)
# evidence_block01 = Term('block', Term('red'),0,1)
# evidence_block11 = Term('block', Term('blue'),1,1)
# evidence_block21 = Term('block', Term('red'),2,1)
# evidence_block00 = Term('block', Term('red'),0,0)
# evidence_block10 = Term('block', Term('red'),1,0)
# evidence_block20 = Term('block', Term('yellow'),2,0)

query_term = Term('initial_board', None)

lf = engine.ground_all(db,queries=[query_term])
print(get_evaluatable().create_from(lf).evaluate())

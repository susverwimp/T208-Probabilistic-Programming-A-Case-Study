from __future__ import print_function
import csv

from problog.program import PrologString
from problog.engine import DefaultEngine
from problog.logic import Constant,Var,Term

p = PrologString("""
:- use_module(library(apply)).
:- use_module(library(lists)).

pressable_color(red).
pressable_color(green).
pressable_color(blue).
pressable_color(yellow).

board(Board) :-
    pressable_color(Color00),
    pressable_color(Color10),
    pressable_color(Color20),
    pressable_color(Color01),
    pressable_color(Color11),
    pressable_color(Color21),
    pressable_color(Color02),
    pressable_color(Color12),
    pressable_color(Color22),
    Board = [[block(Color02,0,2),block(Color12,1,2),block(Color22,2,2)],
             [block(Color01,0,1),block(Color11,1,1),block(Color21,2,1)],
             [block(Color00,0,0),block(Color10,1,0),block(Color20,2,0)]
             ],
    check_valid_board(Board).

check_valid_board(Board) :-
    check_horizontal(Board),
    check_vertical(Board).
    
check_horizontal([]).
check_horizontal([Row|Tail]) :-
    check_list(Row),
    check_horizontal(Tail).
    
check_vertical(Board) :-
    transpose(Board,Transpose),
    check_horizontal(Transpose).
    
check_list([]).
check_list([block(Color,_,_)|Tail]) :-
    check_list(Tail,[Color]).
check_list([],Blocks) :-
    length(Blocks,N),
    N < 3.
check_list([block(Color1,_,_)|Tail1],[Color2|Tail2]) :-
    Color1 = Color2,
    length([Color2|Tail2],N),
    N < 3,
    NewBlocks = [Color1|[Color2|Tail2]],
    check_list(Tail1,NewBlocks).
check_list([block(Color1,_,_)|Tail1],[Color2|Tail2]) :-
    Color1 \= Color2,
    length([Color2|Tail2],N),
    N < 3,
    NewBlocks = [Color1],
    check_list(Tail1,NewBlocks).

transpose(Ls, Ts) :-
	lists_transpose(Ls, Ts).

lists_transpose([], []).
lists_transpose([L|Ls], Ts) :-
	maplist(same_length(L), Ls),
	foldl(transpose_, L, Ts, [L|Ls], _).

transpose_(_, Fs, Lists0, Lists) :-
	maplist(list_first_rest, Lists0, Fs, Lists).
	
list_first_rest([L|Ls], L, Ls).

same_length([],[]).
same_length([_|L1],[_|L2]) :- same_length(L1, L2).
""")

def main():

    engine = DefaultEngine(label_all=True)

    db = engine.prepare(p)
    query_board = Term('board', None)
    a = engine.query(db,query_board)
    with open("3x3.csv", "wb") as f:
        writer = csv.writer(f)
        writer.writerows(a)

if __name__ == '__main__':
    main()
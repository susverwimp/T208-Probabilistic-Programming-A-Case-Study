:- use_module(library(apply)).
:- use_module(library(lists)).

position(X,Y) :-
    width(Width),
    height(Height),
    between(0,Width,X),
    between(0,Height,Y).

pressable_color(red).
pressable_color(green).
pressable_color(blue).
pressable_color(yellow).

%%%%%%%%%%%%%%%%%
% RANDOM EVENTS
%%%%%%%%%%%%%%%%%
1/4::uniform_color(red,X,Y);1/4::uniform_color(green,X,Y);1/4::uniform_color(blue,X,Y);1/4::uniform_color(yellow,X,Y).

block(Color,X,Y) :-
    position(X,Y),
    uniform_color(Color,X,Y).

probs_color_change(red,green,1/3). probs_color_change(red,blue,1/3). probs_color_change(red,yellow,1/3).
probs_color_change(green,red,1/3). probs_color_change(green,blue,1/3). probs_color_change(green,yellow,1/3).
probs_color_change(blue,red,1/3). probs_color_change(blue,green,1/3). probs_color_change(blue,yellow,1/3).
probs_color_change(yellow,red,1/3). probs_color_change(yellow,green,1/3). probs_color_change(yellow,blue,1/3).

select_uniform(block(Color,X,Y),[],false).
select_uniform(block(Color,X,Y),[block(Color,X,Y)],true).
P::select_uniform(block(Color,X,Y),[block(Color,X,Y)|Tail],true);PRem::select_uniform(B2,[block(Color,X,Y)|Tail],true) :-
    length([block(Color,X,Y)|Tail],N),
    P is 1/N,
    PRem is 1 - P,
    Tail \= [],
    select_uniform(B2,Tail,true).

press(Board,X,Y,Color,T,SomeThingSelected) :-
    strategy(Strategy),
    press(Board,X,Y,Color,T,Strategy,SomeThingSelected).

press(Board,X,Y,Color,T,uniform,SomeThingSelected) :-
    flatten(Board,FlattenBoard),
    remove_whites(FlattenBoard,FlattenBoardNoWhites),
    select_uniform(block(Color,X,Y),FlattenBoardNoWhites,SomeThingSelected).

remove_whites(FlattenBoard,FlattenBoardNoWhites) :-
    remove_whites(FlattenBoard,FlattenBoardNoWhites,[]).
remove_whites([],FlattenBoardNoWhites,FlattenBoardNoWhites).
remove_whites([block(white,_,_)|Tail],FlattenBoardNoWhites,FlattenBoardNoWhitesAcc) :-
    remove_whites(Tail,FlattenBoardNoWhites,FlattenBoardNoWhitesAcc).
remove_whites([block(Color,X,Y)|Tail],FlattenBoardNoWhites,FlattenBoardNoWhitesAcc) :-
    Color \= white,
    append(FlattenBoardNoWhitesAcc,[block(Color,X,Y)],NewFlattenBoardNoWhitesAcc),
    remove_whites(Tail,FlattenBoardNoWhites,NewFlattenBoardNoWhitesAcc).

press(Board,X,Y,Color,T,color_ratio,SomeThingSelected) :-
    flatten(Board,FlattenBoard),
    list_ratio(FlattenBoard,ColorRatio),
    remove_zero_ratio(ColorRatio,NonZeroColorRatio),
    min_colors(NonZeroColorRatio,Colors),
    findall(block(Color,X,Y),(member(Color,Colors),member(block(Color,X,Y),FlattenBoard)),Blocks),
    select_uniform(block(Color,X,Y),Blocks,SomeThingSelected).


press(Board,X,Y,Color,T,possible_score,SomeThingSelected) :-
    find_all_possible_score_blocks(Board,Blocks),
    possible_score_press(Blocks,Board,Color,X,Y,SomeThingSelected).

% if possible_score_blocks is empty, use uniform press.
possible_score_press([],Board,Color,X,Y,SomeThingSelected) :-
    flatten(Board,FlattenBoard),
    remove_whites(FlattenBoard,FlattenBoardNoWhites),
    select_uniform(block(Color,X,Y),FlattenBoardNoWhites,SomeThingSelected).
possible_score_press(Blocks,Board,Color,X,Y,SomeThingSelected) :-
    Blocks \= [],
    select_uniform(block(Color,X,Y),Blocks,SomeThingSelected).

press(Board,X,Y,Color,T,possible_score_improved,SomeThingSelected) :-
    find_all_possible_score_blocks(Board,Blocks),
    possible_score_improved_press(Blocks,Board,Color,X,Y,SomeThingSelected).

possible_score_improved_press([],Board,Color,X,Y,SomeThingSelected) :-
    flatten(Board,FlattenBoard),
    remove_whites(FlattenBoard,FlattenBoardNoWhites),
    select_uniform(block(Color,X,Y),FlattenBoardNoWhites,SomeThingSelected).
possible_score_improved_press(Blocks,Board,Color,X,Y,SomeThingSelected) :-
    Blocks \= [],
    find_max_score_blocks(Board,Blocks,MaxScoreBlocks),
    select_uniform(block(Color,X,Y),MaxScoreBlocks,SomeThingSelected).

find_max_score_blocks(Board,Blocks,MaxScoreBlocks) :-
    find_max_score_blocks(Board,Blocks,MaxScoreBlocks,0,[]).
find_max_score_blocks(_,[],MaxScoreBlocks,_,MaxScoreBlocks).
find_max_score_blocks(Board,[block(Color,X,Y)|Tail],MaxScoreBlocks,WeightedScore,_) :-
    find_weighted_score_block(Board,block(Color,X,Y),BlockWeightedScore),
    BlockWeightedScore > WeightedScore,
    NewMaxScoreBlocksAcc = [block(Color,X,Y)],
    find_max_score_blocks(Board,Tail,MaxScoreBlocks,BlockWeightedScore,NewMaxScoreBlocksAcc).
find_max_score_blocks(Board,[block(Color,X,Y)|Tail],MaxScoreBlocks,WeightedScore,MaxScoreBlocksAcc) :-
    find_weighted_score_block(Board,block(Color,X,Y),BlockWeightedScore),
    BlockWeightedScore = WeightedScore,
    NewMaxScoreBlocksAcc = [block(Color,X,Y)|MaxScoreBlocksAcc],
    find_max_score_blocks(Board,Tail,MaxScoreBlocks,BlockWeightedScore,NewMaxScoreBlocksAcc).
find_max_score_blocks(Board,[block(Color,X,Y)|Tail],MaxScoreBlocks,WeightedScore,MaxScoreBlocksAcc) :-
    find_weighted_score_block(Board,block(Color,X,Y),BlockWeightedScore),
    BlockWeightedScore < WeightedScore,
    find_max_score_blocks(Board,Tail,MaxScoreBlocks,WeightedScore,MaxScoreBlocksAcc).

find_weighted_score_block(Board,Block,WeightedScore) :-
    findall(C,pressable_color(C),Colors),
    find_weighted_score_block(Board,Block,Colors,WeightedScore,0).
find_weighted_score_block(_,_,[],WeightedScore,WeightedScore).
find_weighted_score_block(Board,block(Color,X,Y),[Color|Tail],WeightedScore,WeightedScoreAcc) :-
    find_weighted_score_block(Board,block(Color,X,Y),Tail,WeightedScore,WeightedScoreAcc).
find_weighted_score_block(Board,block(Color1,X,Y),[Color2|Tail],WeightedScore,WeightedScoreAcc) :-
    Color1 \= Color2,
    change_color_in_board(Board,X,Y,Color2,ColorChangedBoard),
    remove_and_drop(ColorChangedBoard, _, CurrentScore),
    probs_color_change(Color1,Color2,Probs),
    NewWeightedScoreAcc is WeightedScoreAcc + (CurrentScore * Probs),
    find_weighted_score_block(Board,block(Color1,X,Y),Tail,WeightedScore,NewWeightedScoreAcc).

how_many_blocks_with_color(Board, Blocks) :-
    how_many_blocks_with_color(Board, Blocks, 0).
how_many_blocks_with_color([], Blocks, Blocks).
how_many_blocks_with_color([[]|Tail], Blocks, Acc) :-
    how_many_blocks_with_color(Tail, Blocks, Acc).
how_many_blocks_with_color([[block(Color, _, _)|Tail1]|Tail2], Blocks, Acc) :-
    pressable_color(Color),
    NewAcc is Acc + 1,
    how_many_blocks_with_color([Tail1|Tail2], Blocks, NewAcc).
how_many_blocks_with_color([[block(Color, _, _)|Tail1]|Tail2], Blocks, Acc) :-
    \+ pressable_color(Color),
    how_many_blocks_with_color([Tail1|Tail2], Blocks, Acc).

find_block_in_board(block(Color,X,Y),Board) :-
    member(Row, Board),
    member(block(Color, X, Y), Row).


remove_zero_ratio(List,Result) :-
    remove_zero_ratio(List,Result,[]).
remove_zero_ratio([],Result, Result).
remove_zero_ratio([[_,0]|Tail],Result, ResultAcc) :-
    remove_zero_ratio(Tail,Result,ResultAcc).
remove_zero_ratio([[Color,Count]|Tail],Result, ResultAcc) :-
    Count > 0,
    append(ResultAcc,[[Color,Count]], NewResultAcc),
    remove_zero_ratio(Tail,Result,NewResultAcc).

min_colors([],[]).
min_colors([[Color,Count]|Tail],MinColors) :-
    min_colors(Tail,MinColors,[Color],Count).
min_colors([],MinColors,MinColors,MinCount).
min_colors([[Color,Count]|Tail],MinColors,MinColorsAcc,MinCount) :-
    Count < MinCount,
    min_colors(Tail,MinColors,[Color],Count).
min_colors([[Color,Count]|Tail],MinColors,MinColorsAcc,MinCount) :-
    Count = MinCount,
    NewMinColorsAcc = [Color|MinColorsAcc],
    min_colors(Tail,MinColors,NewMinColorsAcc,MinCount).
min_colors([[Color,Count]|Tail],MinColors,MinColorsAcc,MinCount) :-
    Count > MinCount,
    min_colors(Tail,MinColors,MinColorsAcc,MinCount).

find_all_possible_score_blocks(Board,Blocks) :-
    transpose(Board,TransposeBoard),
    findall(Block,find_possible_score_block_horizontal(Board,Block),BlocksHorizontal),
    findall(Block,find_possible_score_block_horizontal(TransposeBoard,Block),BlocksVertical),
    append(BlocksHorizontal,BlocksVertical,ListBlocks),
    set(ListBlocks,Blocks).

find_possible_score_block_horizontal(Board,Block) :-
    member(Row,Board),
    member(Block,Row),
    has_possible_score(Block,Row).

has_possible_score(block(Color,X,Y),List) :-
    pressable_color(Color),
    X2 is X + 1,
    X3 is X + 2,
    member(block(Color2,X2,Y),List),
    pressable_color(Color2),
    Color2 \= Color,
    member(block(Color3,X3,Y),List),
    Color3 == Color2.
has_possible_score(block(Color,X,Y),List) :-
    pressable_color(Color),
    X2 is X - 1,
    X3 is X - 2,
    member(block(Color2,X2,Y),List),
    pressable_color(Color2),
    Color2 \= Color,
    member(block(Color3,X3,Y),List),
    Color3 == Color2.
has_possible_score(block(Color,X,Y),List) :-
    pressable_color(Color),
    X2 is X + 1,
    X3 is X - 1,
    member(block(Color2,X2,Y),List),
    pressable_color(Color2),
    Color2 \= Color,
    member(block(Color3,X3,Y),List),
    Color3 == Color2.
has_possible_score(block(Color,X,Y),List) :-
    pressable_color(Color),
    Y2 is Y + 1,
    Y3 is Y + 2,
    member(block(Color2,X,Y2),List),
    pressable_color(Color2),
    Color2 \= Color,
    member(block(Color3,X,Y3),List),
    Color3 == Color2.
has_possible_score(block(Color,X,Y),List) :-
    pressable_color(Color),
    Y2 is Y - 1,
    Y3 is Y - 2,
    member(block(Color2,X,Y2),List),
    pressable_color(Color2),
    Color2 \= Color,
    member(block(Color3,X,Y3),List),
    Color3 == Color2.
has_possible_score(block(Color,X,Y),List) :-
    pressable_color(Color),
    Y2 is Y + 1,
    Y3 is Y - 1,
    member(block(Color2,X,Y2),List),
    pressable_color(Color2),
    Color2 \= Color,
    member(block(Color3,X,Y3),List),
    Color3 == Color2.

list_ratio(List, ListRatio) :-
    list_ratio(List, ListRatio, 0, 0, 0, 0).
list_ratio([], [[red,RedAcc], [green,GreenAcc], [blue,BlueAcc], [yellow,YellowAcc]], RedAcc, GreenAcc, BlueAcc, YellowAcc).
list_ratio([block(Color,_,_)|Tail], ListRatio, RedAcc, GreenAcc, BlueAcc, YellowAcc) :-
    Color = red,
    NewRedAcc is RedAcc + 1,
    list_ratio(Tail, ListRatio, NewRedAcc, GreenAcc, BlueAcc, YellowAcc).
list_ratio([block(Color,_,_)|Tail], ListRatio, RedAcc, GreenAcc, BlueAcc, YellowAcc) :-
    Color = green,
    NewGreenAcc is GreenAcc + 1,
    list_ratio(Tail, ListRatio, RedAcc, NewGreenAcc, BlueAcc, YellowAcc).
list_ratio([block(Color,_,_)|Tail], ListRatio, RedAcc, GreenAcc, BlueAcc, YellowAcc) :-
    Color = blue,
    NewBlueAcc is BlueAcc + 1,
    list_ratio(Tail, ListRatio, RedAcc, GreenAcc, NewBlueAcc, YellowAcc).
list_ratio([block(Color,_,_)|Tail], ListRatio, RedAcc, GreenAcc, BlueAcc, YellowAcc) :-
    Color = yellow,
    NewYellowAcc is YellowAcc + 1,
    list_ratio(Tail, ListRatio, RedAcc, GreenAcc, BlueAcc, NewYellowAcc).
list_ratio([block(Color,_,_)|Tail], ListRatio, RedAcc, GreenAcc, BlueAcc, YellowAcc) :-
    Color = white,
    list_ratio(Tail, ListRatio, RedAcc, GreenAcc, BlueAcc, YellowAcc).

RG::change_color(red,green,T);RB::change_color(red,blue,T);RY::change_color(red,yellow,T) :-
    probs_color_change(red,green,RG),
    probs_color_change(red,blue,RB),
    probs_color_change(red,yellow,RY).
GR::change_color(green,red,T);GB::change_color(green,blue,T);GY::change_color(green,yellow,T) :-
    probs_color_change(green,red,GR),
    probs_color_change(green,blue,GB),
    probs_color_change(green,yellow,GY).
BR::change_color(blue,red,T);BG::change_color(blue,green,T);BY::change_color(blue,yellow,T) :-
    probs_color_change(blue,red,BR),
    probs_color_change(blue,green,BG),
    probs_color_change(blue,yellow,BY).
YR::change_color(yellow,red,T);YG::change_color(yellow,green,T);YB::change_color(yellow,blue,T) :-
    probs_color_change(yellow,red,YR),
    probs_color_change(yellow,green,YG),
    probs_color_change(yellow,blue,YB).


%%%%%%%%%%%%%%%%%
% GAME PREDICATES
%%%%%%%%%%%%%%%%%
board(0,Board,0,[]) :-
    initial_board(Board).
board(T,Board,Score,Positions) :-
    T > 0,
    TT is T - 1,
    board(TT,PreviousBoard,PreviousScore,PreviousPositions),
    press(PreviousBoard,X,Y,Color,TT,true),
    append(PreviousPositions,[[X,Y]],Positions),
    change_color(Color,NewColor,TT),
    change_color_in_board(PreviousBoard,X,Y,NewColor,ColorChangedBoard),
    remove_and_drop(ColorChangedBoard, Board, CurrentScore),
    Score is PreviousScore + CurrentScore.

board(T,Board,Score,Positions) :-
    T > 0,
    TT is T - 1,
    board(TT,Board,Score,Positions),
    press(Board,X,Y,Color,TT,false).

score_of_turn(T,Score) :-
    board(T,Board,Score,Positions).

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

change_color_in_board(Board, X, Y, NewColor, NewBoard) :-
    change_color_in_board(Board, X, Y, NewColor, NewBoard, []).
change_color_in_board([], _, _, _, NewBoard, NewBoard).
change_color_in_board([Row|Tail], X, Y, NewColor, NewBoard, NewBoardAcc) :-
    \+member(block(_, X, Y), Row),
    append(NewBoardAcc,[Row], NewNewBoardAcc),
    change_color_in_board(Tail, X, Y, NewColor, NewBoard, NewNewBoardAcc).
change_color_in_board([Row|Tail], X, Y, NewColor, NewBoard, NewBoardAcc) :-
    member(block(_, X, Y), Row),
    change_color_in_row(Row, X, NewColor, NewRow),
    append(NewBoardAcc,[NewRow|Tail], NewNewBoardAcc),
    change_color_in_board([], X, Y, NewColor, NewBoard, NewNewBoardAcc).

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

remove_and_drop(Board, NewBoard, Score) :-
    remove_and_drop(Board, NewBoard, Score, 0).
remove_and_drop(Board, Board, Score, Score) :-
    find_same(Board, []).
remove_and_drop(Board, NewBoard, Score, ScoreAcc) :-
    find_same(Board, Same),
    length(Same, Length),
    Length > 0,
    NewScoreAcc is ScoreAcc + Length,
    remove_same(Board, Same, BoardAcc),
    drop_blocks(BoardAcc, NewBoardAcc),
    remove_and_drop(NewBoardAcc, NewBoard, Score, NewScoreAcc).

remove_same(Board, [], Board).
remove_same(Board, [block(_, X, Y)|Tail], NewBoard) :-
    change_color_in_board(Board, X, Y, white, BoardAcc),
    remove_same(BoardAcc, Tail, NewBoard).

drop_blocks(Board, NewBoard) :-
    transpose(Board, Transpose),
    drop_columns(Transpose, NewBoardTranspose),
    transpose(NewBoardTranspose, NewBoard).

drop_columns(Board, NewBoard) :-
    drop_columns(Board, NewBoard,[]).
drop_columns([],NewBoard,NewBoard).
drop_columns([Column|Tail],NewBoard,NewBoardAcc) :-
    drop_column(Column,NewColumn),
    append(NewBoardAcc,[NewColumn],NewNewBoardAcc),
    drop_columns(Tail,NewBoard,NewNewBoardAcc).

drop_column([block(Color,X,Y)|Tail],NewColumn) :-
    length([block(Color,X,Y)|Tail],L),
    get_colors_of_column_in_order([block(Color,X,Y)|Tail],Colors),
    create_new_column_of_colors(Colors,L,X,NewColumn).

get_colors_of_column_in_order(Column,Colors) :-
    get_colors_of_column_in_order(Column,Colors,[]).
get_colors_of_column_in_order([],Colors,Colors).
get_colors_of_column_in_order([block(Color,_,_)|Tail],Colors,ColorsAcc) :-
    pressable_color(Color),
    get_colors_of_column_in_order(Tail,Colors,[Color|ColorsAcc]).
get_colors_of_column_in_order([block(Color,_,_)|Tail],Colors,ColorsAcc) :-
    \+ pressable_color(Color),
    get_colors_of_column_in_order(Tail,Colors,ColorsAcc).

create_new_column_of_colors(Colors,Height,X,NewColumn) :-
    create_new_column_of_colors(Colors,Height,X,NewColumn,[],0).
create_new_column_of_colors([],Height,_,NewColumn,NewColumn,Height).
create_new_column_of_colors([],Height,X,NewColumn,NewColumnAcc,Index) :-
    Height > Index,
    NewIndex is Index + 1,
    create_new_column_of_colors([],Height,X,NewColumn,[block(white,X,Index)|NewColumnAcc],NewIndex).
create_new_column_of_colors([Color|Tail],Height,X,NewColumn,NewColumn,Height).
create_new_column_of_colors([Color|Tail],Height,X,NewColumn,NewColumnAcc,Index) :-
    Height > Index,
    NewIndex is Index + 1,
    create_new_column_of_colors(Tail,Height,X,NewColumn,[block(Color,X,Index)|NewColumnAcc],NewIndex).

find_same(Board, Same) :-
    find_same_horizontal(Board, Horizontal),
    find_same_vertical(Board, Vertical),
    append(Horizontal, Vertical, SameAcc),
    set(SameAcc, Same).

find_same_horizontal(Board, Same) :-
    find_same_horizontal(Board, Same, []).
find_same_horizontal([], Same, Same).
find_same_horizontal([Row|Tail], Same, Acc) :-
    find_same_in_list(Row, SameInRow),
    append(Acc, SameInRow, NewAcc),
    find_same_horizontal(Tail, Same, NewAcc).

find_same_vertical(Board, Same) :-
    transpose(Board, ColumnBoard),
    find_same_horizontal(ColumnBoard, Same, []).

find_same_in_list(List, Same) :-
    pack(List, PackedList),
    remove_no_colors_from_packed_list(PackedList, NoColorPackedList),
    find_same_in_packed_list(NoColorPackedList, Same, []).
find_same_in_packed_list([], Same, Same).
find_same_in_packed_list([Pack|Tail], Same, Acc) :-
    length(Pack, Length),
    Length > 2,
    append(Acc, Pack, NewAcc),
    find_same_in_packed_list(Tail, Same, NewAcc).
find_same_in_packed_list([Pack|Tail], Same, Acc) :-
    length(Pack, Length),
    Length =< 2,
    find_same_in_packed_list(Tail, Same, Acc).

remove_no_colors_from_packed_list(PackedList, Result) :-
    remove_no_colors_from_packed_list(PackedList, Result, []).
remove_no_colors_from_packed_list([], Result, Result).
remove_no_colors_from_packed_list([[block(Color,X,Y)|Tail1]|Tail2], Result, ResultAcc) :-
    pressable_color(Color),
    append(ResultAcc, [[block(Color,X,Y)|Tail1]], NewResultAcc),
    remove_no_colors_from_packed_list(Tail2, Result, NewResultAcc).
remove_no_colors_from_packed_list([[block(Color,_,_)|_]|Tail2], Result, ResultAcc) :-
    \+ pressable_color(Color),
    remove_no_colors_from_packed_list(Tail2, Result, ResultAcc).


%%%%%%%%%%%%%%%%%
% UTILITY PREDICATES
%%%%%%%%%%%%%%%%%
% flatten a list: [[a],[b]] = [a,b]
flatten([],[]).
flatten([X|Xs],Zs) :- flatten(X,Y), flatten(Xs,Ys), append(Y,Ys,Zs).

% transpose a 2D board
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

% insert element at last position in list
insert_at_end(X,[],[X]).
insert_at_end(X,[H|T],[H|Z]) :- insert_at_end(X,T,Z).

% pack a list: [a,a,a,a,b,b,c,c,a,b] = [[a,a,a,a],[b,b],[c,c],[a],[b]]
pack([],[]).
pack([X|Xs],[Z|Zs]) :- transfer(X,Xs,Ys,Z), pack(Ys,Zs).

transfer(X,[],[],[X]).
transfer(block(Color1,X1,Y1),[block(Color2,X2,Y2)|Ys],[block(Color2,X2,Y2)|Ys],[block(Color1,X1,Y1)]) :- Color1 \= Color2.
transfer(block(Color,X1,Y1),[block(Color,X2,Y2)|Xs],Ys,[block(Color,X1,Y1)|Zs]) :- transfer(block(Color,X2,Y2),Xs,Ys,Zs).

% Create a set of a list, remove duplicates
set([], []).
set([H|T], [H|T1]) :-
    remv(H, T, T2),
    set(T2, T1).

remv(_, [], []).
remv(X, [X|T], T1) :- remv(X, T, T1).
remv(X, [H|T], [H|T1]) :-
    X \= H,
    remv(X, T, T1).
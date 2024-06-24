% A* Search algo
search(Open, Closed, Goal):-
getBestState(Open, [CurrentState,Parent,G,H,F], _), % Step 1
CurrentState = Goal, % Step 2
write("Search is complete!"), nl,
printSolution([CurrentState,Parent,G,H,F], Closed), !.

search(Open, Closed, Goal):-
getBestState(Open, CurrentNode, TmpOpen),
getAllValidChildren(CurrentNode,TmpOpen,Closed,Goal,Children), % Step3
addChildren(Children, TmpOpen, NewOpen), % Step 4
append(Closed, [CurrentNode], NewClosed), % Step 5.1
search(NewOpen, NewClosed, Goal). % Step 5.2

% Implementation of step 3 to get the next states
getAllValidChildren(Node, Open, Closed, Goal, Children):-
findall(Next, getNextState(Node,Open,Closed,Goal,Next),
Children).

getNextState([State,_,G,_,_],Open,Closed,Goal,[Next,State,NewG,NewH,NewF]):-
move(State, Next, MoveCost),
isOkay(Next),
calculateH(Next, Goal, NewH),
NewG is G + MoveCost,
NewF is NewG + NewH,
(not(member([Next,_,_,_,_], Open)); memberButBetter(Next,Open,NewF)),
(not(member([Next,_,_,_,_], Closed)); memberButBetter(Next,closed,NewF)),
sameColor(_,State,Next).

memberButBetter(Next, List, NewF):-
findall(F, member([Next,_,_,_,F],List),Numbers),
min_list(Numbers,MinOldF),
MinOldF > NewF.
 
% Implementation of addChildren and getBestState
addChildren(Children, Open, NewOpen) :-
    append(Open, Children, NewOpen).

getBestState(Open, BestChild, Rest) :-
    findMin(Open, BestChild),
    delete(Open, BestChild, Rest).

findMin([X], X) :- !.
findMin([Head|T], Min) :-
    findMin(T, TmpMin),
    Head = [_, _, _, HeadH, _],
    TmpMin = [_, _, _, TmpH, _],
    (HeadH =< TmpH -> Min = TmpMin ; Min = Head).

% Implementation of findMin in getBestState determines the search alg.
printSolution([State, null, G, H, F], _) :-
    write([State, G, H, F]), nl.
printSolution([State, Parent, G, H, F], Closed) :-
    member([Parent, GrandParent, PrevG, Ph, Pf], Closed),
    printSolution([Parent, GrandParent, PrevG, Ph, Pf], Closed),
    write([State, G, H, F]), nl.


% Define moves
% X = rows , Y = Cols
move([X1,Y], [X2,Y], 1):- %Down
rows(Rows),
X2 is X1+1,
X2 < Rows,
sameColor(_,[X1,Y],[X2,Y]).

move([X,Y1], [X,Y2], 1):- %Right
cols(Cols),
Y2 is Y1+1,
Y2 < Cols,
sameColor(_,[X,Y1],[X,Y2]).

move([X,Y1], [X,Y2], 1):- %Up
Y2 is Y1-1,
Y2 >= 0,
sameColor(_,[X,Y1],[X,Y2]).

move([X1,Y], [X2,Y], 1):- %Left
X2 is X1-1,
X2 >= 0,
sameColor(_,[X1,Y],[X2,Y]).

isOkay([X,Y]):- 
rows(Rows), cols(Cols),
X >= 0, X < Rows,
Y >= 0, Y < Cols.

% Check if the color of two cells is the same
sameColor(Color, [X1,Y1], [X2,Y2]):-
    board(Board),
    nth0(X1, Board, Row1),
    nth0(Y1, Row1, Color),
    nth0(X2, Board, Row2),
    nth0(Y2, Row2, Color).

% calc the distance from curr cell to the goal
calculateH([X1,Y1], [X2,Y2], H):-
H is abs(X1 - X2) + abs(Y1 - Y2).


buildBoard(0, _, Board, Board):- !.
buildBoard(Rows, Cols, Acc, Board):-
    NewRows is Rows - 1,
    appendRow(Cols, [], Row),
    append(Acc, [Row], NewAcc),
    buildBoard(NewRows, Cols, NewAcc, Board).

appendRow(0, Row, Row):- !.
appendRow(Cols, Acc, Row):-
    NewCols is Cols - 1,
    write("Enter color for cell "),writeln(" :"),
    read(Color),
    append(Acc, [Color], NewAcc),
    appendRow(NewCols, NewAcc, Row).


start(Color,Start):-
isOkay(Start),
sameColor(Color,Start,_).

goal(Color,Goal):-
isOkay(Goal),
sameColor(Color,Goal,_).

startSearch:-
    writeln("Enter # of rows like 4.: "), 
    read(Rows),
    assert(rows(Rows)),
    writeln("Enter # of cols like 4.: "),
    read(Cols),
    assert(cols(Cols)),
    buildBoard(Rows, Cols, [], Board),
    assert(board(Board)),
    writeln("Enter color of start and goal cell: "),
    read(Color),
    writeln("Enter pos of the start cell like [0,0]. : "),
    read(Start),
    writeln("Enter pos of the goal cell like [0,3]. : "),
    read(Goal),
    start(Color, Start),
    goal(Color, Goal), 
    search([[Start, null, 0, 0, 0]], [], Goal).

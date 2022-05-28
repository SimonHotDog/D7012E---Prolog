
/* ------------------------------------------------------- */
%
%    D7012E Declarative languages
%    Luleå University of Technology
%
%    Student full name: Simon Lundberg
%    Student user id  : lunsim-8 
%
/* ------------------------------------------------------- */

:- ensure_loaded('play.pl').
% :- ensure_loaded('stupid.pl'). % uncomment to make stupid computer replace the human player.

% /* ------------------------------------------------------ */
%               IMPORTANT! PLEASE READ THIS SUMMARY:
%       This files gives you some useful helpers (set & get).
%       Your job is to implement several predicates using
%       these helpers. Feel free to add your own helpers if
%       needed, as long as you write comments (documentation)
%       for all of them. 
%
%       Implement the following predicates at their designated
%       space in this file. You might like to have a look at
%       the file  ttt.pl  to see how the implementations is
%       done for game tic-tac-toe.
%
%          * initialize(InitialState,InitialPlyr).
%          * winner(State,Plyr) 
%          * tie(State)
%          * terminal(State) 
%          * moves(Plyr,State,MvList)
%          * nextState(Plyr,Move,State,NewState,NextPlyr)
%          * validmove(Plyr,State,Proposed)
%          * h(State,Val)  (see question 2 in the handout)
%          * lowerBound(B)
%          * upperBound(B)
%
%
% We use the following State Representation: 
% [Row0, Row1 ... Rown] (ours is 6x6 so n = 5 ).
% each Rowi is a LIST of 6 elements '.' or '1' or '2' as follows: 
%    . means the position is  empty
%    1 means player one has a stone in this position
%    2 means player two has a stone in this position. 


% given helper: Inital state of the board

cord(C) :- C = [n, ne, e, se, s, sw, w, nw].

initBoard([ [.,.,.,.,.,.], 
            [.,.,.,.,.,.],
	    	[.,.,1,2,.,.], 
	    	[.,.,2,1,.,.], 
            [.,.,.,.,.,.], 
	    	[.,.,.,.,.,.] ]).

termtest([  [2,2,2,2,2,2], 
            [2,2,2,1,2,2],
	    	[2,1,2,2,2,2], 
	    	[2,1,2,1,2,1], 
            [2,2,1,2,1,.], 
	    	[2,1,1,1,1,.]  ]).

%%%%%%%%%%%%%%%%%% initialize(...) %%%%%%%%%%%%%%%%%%%%%%%%
%%% Using initBoard define initialize(InitialState,InitialPlyr). 
%%%  holds iff InitialState is the initial state and 
%%%  InitialPlyr is the player who moves first. 

%initialize(InitialState, 1) :- initBoard(InitialState).
initialize(InitialState, 1) :- termtest(InitialState).

%%%%%%%%%%%%%%%%%% winner(...) %%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define winner(State,Plyr) here.  
%     - returns winning player if State is a terminal position and
%     Plyr has a lower score than the other player 

winner(State, Plyr) :-
	terminal(State),
	calcScore(State, Player1, Player2),
	(Player1 < Player2, Plyr = 1;
	 Player1 > Player2, Plyr = 2).

%% calcScore(State, Player1, Player2).
%		- returns the score of current state for player1 and player2
calcScore(State, Player1, Player2) :- 
	flatten(State, Squares),
	calcScoreInner(1, Squares, Player1),
	calcScoreInner(2, Squares, Player2).

calcScoreInner(_, [], 0) :- !.
calcScoreInner(Plyr,[Plyr|Squares], Score) :- 
	calcScoreInner(Plyr, Squares, S),
	Score is S + 1, !.
calcScoreInner(Plyr,[_|Squares], Score) :- 
	calcScoreInner(Plyr, Squares, Score).


%%%%%%%%%%%%%%%%%% tie(...) %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define tie(State) here. 
%    - true if terminal State is a "tie" (no winner) 

tie(State) :- 
	terminal(State), 
	calcScore(State, Score, Score), !.


%%%%%%%%%%%%%%%%%% terminal(...) %%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define terminal(State). 
%   - true if State is a terminal

terminal(State) :-
	moves(1, State, Moves1), Moves1 = [n],
	moves(2, State, Moves2), Moves2 = [n].


%%%%%%%%%%%%%%%%%% showState(State) %%%%%%%%%%%%%%%%%%%%%%%
%% given helper. Do not change this. It's used by play.pl
%%

showState( G ) :- 
	printRows( G ). 
 
printRows( [] ). 
printRows( [H|L] ) :- 
	printList(H),
	nl,
	printRows(L). 

printList([]).
printList([H | L]) :-
	write(H),
	write(' '),
	printList(L).


%%%%%%%%%%%%%%%%%% moves(Plyr,State,MvList) %%%%%%%%%%%%%%%
%% 
%% define moves(Plyr,State,MvList). 
%   - returns list MvList of all legal moves Plyr can make in State

moves(Plyr, State, MvList) :- 
	getAllCords(Cs),
	movesInner(Plyr, State, Valid, Cs),
	(Valid = [] -> MvList = [n], ! ; MvList = Valid).

movesInner(_, _, [], []).
movesInner(Plyr, State, [C|MvList], [C|Cs]) :-
	validmove(Plyr, State, C),
	movesInner(Plyr, State, MvList, Cs), !.

movesInner(Plyr, State, MvList, [_|Cs]) :- movesInner(Plyr, State, MvList, Cs).


%%%%%%%%%%%%%%%%%% nextState(Plyr,Move,State,NewState,NextPlyr) %%%%%%%%%%%%%%%%%%%%
%% 
%% define nextState(Plyr,Move,State,NewState,NextPlyr). 
%   - given that Plyr makes Move in State, it determines NewState (i.e. the next 
%     state) and NextPlayer (i.e. the next player who will move).

nextState(Plyr, pass, State, State, NextPlyr) :- getEnemy(Plyr, NextPlyr), !.
nextState(Plyr, Move, State, NewState, NextPlyr) :-
	getEnemy(Plyr, NextPlyr), cord(C),
	flipper(C, Plyr, State, Move, NewState).


flipper([], _, State, _, State) :- !.
flipper([Dir|Dirs], Plyr, State, Position, NextState) :-
	flip(Dir, Plyr, State, Position, InterState),
	flipper(Dirs, Plyr, InterState, Position, NextState), !.
flipper([_|Dirs], Plyr, State, Position, NextState) :-
	flipper(Dirs, Plyr, State, Position, NextState).


flip(Dir, Plyr, State, Pos, NextState) :-
	inTheWay(Dir, Plyr, State, Pos, S),
	FlipNumber is S + 1,
	flipInner(Dir, Plyr, State, Pos, FlipNumber, NextState).

flipInner(_, _, State, _, 0, State).
flipInner(Dir, Plyr, State, Pos, FlipNumber, NextState) :-
	FlipNumber > 0,
	AfterFlip is FlipNumber - 1,
	set(State, InterState, Pos, Plyr),
	movedir(Dir, Pos, NextPos),
	flipInner(Dir, Plyr, InterState, NextPos, AfterFlip, NextState), !.


%%%%%%%%%%%%%%%%%% validmove(Plyr,State,Proposed) %%%%%%%%%
%% 
%% define validmove(Plyr,State,Proposed). 
%   - true if Proposed move by Plyr is valid at State.

validmove(Plyr, State, Proposed) :- 
    get(State, Proposed, Square), Square = '.', cord(C),
    validmoveInner(C, Plyr, State, Proposed).
validmove(Plyr, State, pass) :- moves(Plyr, State, Moves), Moves = [n], !.

validmoveInner([], , , _) :- !, fail.
validmoveInner([C|_], Plyr, State, Proposed) :-
    inTheWay(C, Plyr, State, Proposed, S), S > 0, !.
validmoveInner([_|CS], Plyr, State, Proposed) :- validmoveInner(CS, Plyr, State, Proposed).

	


% Checks stones that are inbetween the player and the proposed move. returns score

inTheWay(Dir, Plyr, State, Proposed, Score) :-
	isOnTheBoard(Proposed),
	movedir(Dir, Proposed, [NewX, NewY]),
	get(State, [NewX, NewY], Square),
	getEnemy(Plyr, Enemy),
	(Square = Enemy, inTheWay(Dir, Plyr, State, [NewX, NewY], S), Score is S + 1, !;
	 Square = Plyr, Score is 0, !;
	 fail).

%%%%%% MOVEMENTS %%%%%%%

%North
movedir(n, [X, Y], [X, NewY]) :- NewY is Y - 1.

%North East
movedir(ne, [X, Y], [NewX, NewY]) :- NewX is X + 1, NewY is Y - 1.

%East
movedir(e, [X, Y], [NewX, Y]) :- NewX is X + 1.

%South East
movedir(se, [X, Y], [NewX, NewY]) :- NewX is X + 1, NewY is Y + 1.

%South
movedir(s, [X, Y], [X, NewY]) :- NewY is Y + 1.

%South West
movedir(sw, [X, Y], [NewX, NewY]) :- NewX is X - 1, NewY is Y + 1.

%West
movedir(w, [X, Y], [NewX, Y]) :- NewX is X -1.

%North West
movedir(nw, [X, Y], [NewX, NewY]) :- NewX is X - 1, NewY is Y - 1.


%%%%%%%%%%%%%%%%%% h(State,Val)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define h(State,Val). 
%   - given State, returns heuristic Val of that state
%   - larger values are good for Max, smaller values are good for Min
%   NOTE1. If State is terminal h should return its true value.
%   NOTE2. If State is not terminal h should be an estimate of
%          the value of state (see handout on ideas about
%          good heuristics.

h(State, 100) :- winner(State, 1), !.
h(State, -100) :- winner(State, 2), !.
h(_, 0).


%%%%%%%%%%%%%%%%%% lowerBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define lowerBound(B).  
%   - returns a value B that is less than the actual or heuristic value
%     of all states.

lowerBound(-101).


%%%%%%%%%%%%%%%%%% upperBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define upperBound(B). 
%   - returns a value B that is greater than the actual or heuristic value
%     of all states.

upperBound(101).


% get(Board, Point, Element)
%    : get the contents of the board at position column X and row Y
% set(Board, NewBoard, [X, Y], Value):
%    : set Value at column X row Y in Board and bind resulting grid to NewBoard
%
% The origin of the board is in the upper left corner with an index of
% [0,0], the upper right hand corner has index [5,0], the lower left
% hand corner has index [0,5], the lower right hand corner has index
% [5,5] (on a 6x6 board).
%
% Example
% ?- initBoard(B), showState(B), get(B, [2,3], Value). 
%. . . . . . 
%. . . . . . 
%. . 1 2 . . 
%. . 2 1 . . 
%. . . . . . 
%. . . . . . 
%
%B = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], 
%     ['.', '.', 1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], 
%     ['.', '.', '.', '.'|...], ['.', '.', '.'|...]]
%Value = 2 
%Yes
%?- 
%
% Setting values on the board
% ?- initBoard(B),  showState(B),set(B, NB1, [2,4], 1),
%         set(NB1, NB2, [2,3], 1),  showState(NB2). 
%
% . . . . . . 
% . . . . . . 
% . . 1 2 . . 
% . . 2 1 . . 
% . . . . . . 
% . . . . . .
% 
% . . . . . . 
% . . . . . . 
% . . 1 2 . . 
% . . 1 1 . . 
% . . 1 . . . 
% . . . . . .
%
%B = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.', 
%1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], ['.', '.', '.', '.'|...], ['.', '.',
% '.'|...]]
%NB1 = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.'
%, 1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], ['.', '.', 1, '.'|...], ['.', '.
%', '.'|...]]
%NB2 = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.'
%, 1, 2, '.', '.'], ['.', '.', 1, 1, '.'|...], ['.', '.', 1, '.'|...], ['.', 
%'.', '.'|...]]
%
%
% get(Board, Point, Element): get the value of the board at position
% column X and row Y (indexing starts at 0).
% Do not change get:

get( Board, [X, Y], Value) :- 
	nth0( Y, Board, ListY), 
	nth0( X, ListY, Value).

% set( Board, NewBoard, [X, Y], Value): set the value of the board at position
% column X and row Y to Value (indexing starts at 0). Returns the new board as
% NewBoard. Do not change set:

set( [Row|RestRows], [NewRow|RestRows], [X, 0], Value) :-
    setInList(Row, NewRow, X, Value). 

set( [Row|RestRows], [Row|NewRestRows], [X, Y], Value) :-
    Y > 0, 
    Y1 is Y-1, 
    set( RestRows, NewRestRows, [X, Y1], Value). 

% setInList( List, NewList, Index, Value): given helper to set. Do not
% change setInList:

setInList( [_|RestList], [Value|RestList], 0, Value). 

setInList( [Element|RestList], [Element|NewRestList], Index, Value) :- 
	Index > 0, 
	Index1 is Index-1, 
	setInList( RestList, NewRestList, Index1, Value).

%% helpers

%% get enemy
getEnemy(1, 2).
getEnemy(2, 1).

%% Checks if the coordinate is on the board
isOnTheBoard([X,Y]) :- between(0, 5, X), between(0, 5, Y).

% Get all coordinates on the board
getAllCords(Coordinates) :- findall([X,Y], isOnTheBoard([X,Y]), Coordinates).
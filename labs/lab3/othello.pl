/* ------------------------------------------------------- */
%
%    D7012E Declarative languages
%    Luleå University of Technology
%
%    Student full name: Simon Lundberg 
%    Student user id  : lunsim-8 
%
/* ------------------------------------------------------- */



%do not chagne the follwoing line!
:- ensure_loaded('play.pl').


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% /* ------------------------------------------------------ */
%               IMPORTANT! PLEASE READ THIS SUMMARY:
%       This files gives you some useful helpers (set &get).
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
% /* ------------------------------------------------------ */







% /* ------------------------------------------------------ */

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% We use the following State Representation: 
% [Row0, Row1 ... Rown] (ours is 6x6 so n = 5 ).
% each Rowi is a LIST of 6 elements '.' or '1' or '2' as follows: 
%    . means the position is  empty
%    1 means player one has a stone in this position
%    2 means player two has a stone in this position. 



% Self defined helpers

calcScore([], _, 0).

calcScore([Row, Rows], Player, Score) :- calcScore(Row, Player, Score1), calcScore(Rows, Player, Score2), Score is Score1 + Score2. % All rows

calcScore([Position | Row], Player, Score) :- Position = Player, calcScore(Row, Player, Score1), Score is 1 + Score1. % Checks position in row. Point if player

calcScore([Position | Row], Player, Score) :- Position \= Player, calcScore(Row, Player, Score). % No score, since not player

% DO NOT CHANGE THE COMMENT BELOW.
%
% given helper: Inital state of the board

initBoard([ [.,.,.,.,.,.], 
            [.,.,.,.,.,.],
	    	[.,.,1,2,.,.], 
	    	[.,.,2,1,.,.], 
            [.,.,.,.,.,.], 
	    	[.,.,.,.,.,.] ]).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%% IMPLEMENT: initialize(...)%%%%%%%%%%%%%%%%%%%%%
%%% Using initBoard define initialize(InitialState,InitialPlyr). 
%%%  holds iff InitialState is the initial state and 
%%%  InitialPlyr is the player who moves first.

initialize(InitialState, 1) :- initBoard(InitialState).




% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%winner(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define winner(State,Plyr) here.  
%     - returns winning player if State is a terminal position and
%     Plyr has a higher score than the other player 

winner(State, 1) :-
	terminal(State),
	calcScore(State, 1, Player1Score),
	calcScore(State, 2, Player2Score),
	Player1Score < Player2Score.

winner(State, 2) :-
	terminal(State),
	calcScore(State, 1, Player1Score),
	calcScore(State, 2, Player2Score),
	Player2Score < Player1Score.


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%tie(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define tie(State) here. 
%    - true if terminal State is a "tie" (no winner) 

tie(State) :-
	terminal(State),
	calcScore(State, 1, Player1Score),
	calcScore(State, 2, Player2Score),
	Player1Score =:= Player2Score.



% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%terminal(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define terminal(State). 
%   - true if State is a terminal

terminal(State) :-
	moves(1, State, Moves1),
	moves(2, State, Moves2),
	Moves1 == [n],
	Moves2 == [n].


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%showState(State)%%%%%%%%%%%%%%%%%%%%%%%%%%
%% given helper. DO NOT  change this. It's used by play.pl
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

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%moves(Plyr,State,MvList)%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define moves(Plyr,State,MvList). 
%   - returns list MvList of all legal moves Plyr can make in State
%

getBoardCords(C) :- findall([X, Y], isOnTheBoard([X, Y]), C).

moves(Plyr, State, MvList) :-
	getBoardCords(C), movesInner(Plyr, State, C, Valid),
		(Valid = [] -> MvList = [n], ! ; MvList = Valid).
	
movesInner(_, _, [], []).
movesInner(Plyr, State, [C|MvList], [C,C]) :-
	validmove(Plyr, State, C), movesInner(Plyr, State, MvList, C), !.
movesInner(Plyr, State, MvList, [_|C]) :- movesInner(Plyr, State, MvList, C). % To skip over invalid move.
	


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%nextState(Plyr,Move,State,NewState,NextPlyr)%%%%%%%%%%%%%%%%%%%%
%% 
%% define nextState(Plyr,Move,State,NewState,NextPlyr). 
%   - given that Plyr makes Move in State, it determines NewState (i.e. the next 
%     state) and NextPlayer (i.e. the next player who will move).
%

nextState(Plyr, n, State, State, NextPlyr) :- isEnemy(Plyr, NextPlyr), !.
nextState(Plyr, Move, State, NewState, NextPlyr) :-
	isEnemy(Plyr, NextPlyr),
	flipper([n, ne, e, se, s, sw, w, nw], Plyr, State, Move, NewState).


flipper([], _, State, _, State) :- !.
flipper([Dir|Dirs], Plyr, State, Pos, NextState) :-
	flip(Dir, Plyr, State, Pos, InterState),
	flipper(Dir, Plyr, InterState, Pos, NextState), !.
flipper([_|Dirs], Plyr, State, Pos, NextState) :-
	flipper(Dirs, Plyr, State, Pos, NextState).


flip(Dir, Plyr, State, Pos, NextState) :-
	getEnemyStones(Dir, Plyr, State, Pos, Score),
	FlipNumber is Score + 1,
	flipInner(Dir, Plyr, State, Pos, FlipNumber, NextState).

flipInner(_, _, State, _, 0, State).
flipInner(Dir, Plyr, State, Pos, FlipNumber, NextState) :-
	FlipNumber > 0,
	AfterFlip is FlipNumber - 1,
	set(State, InterState, Pos, Plyr),
	moveDir(Dir, Pos, NewPos),
	flipInner(Dir, Plyr, InterState, NewPos, AfterFlip, NextState), !.






% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%validmove(Plyr,State,Proposed)%%%%%%%%%%%%%%%%%%%%
%% 
%% define validmove(Plyr,State,Proposed). 
%   - true if Proposed move by Plyr is valid at State.


validmove(Plyr, State, Proposed) :-
	get(State, Proposed, '.'),
	(getEnemyStones(n, Plyr, State, Proposed, N), N > 0, !;
	getEnemyStones(ne, Plyr, State, Proposed, Ne), Ne > 0, !;
	getEnemyStones(e, Plyr, State, Proposed, E), E > 0, !;
	getEnemyStones(se, Plyr, State, Proposed, Se), Se > 0, !;
	getEnemyStones(s, Plyr, State, Proposed, S), S > 0, !;
	getEnemyStones(sw, Plyr, State, Proposed, Sw), Sw > 0, !;
	getEnemyStones(w, Plyr, State, Proposed, W), W > 0, !;
	getEnemyStones(nw, Plyr, State, Proposed, Nw), Nw > 0).

% checks how many stones between player and the placement of a stone
getEnemyStones(Dir, Plyr, State, Proposed, Score) :-
	isOnTheBoard(Proposed),
	moveDir(Dir, Proposed, [X, Y]),
	get(State, [X, Y], Square),
	isEnemy(Plyr, Enemy),
	(Square = Enemy, 
		getEnemyStones(Dir, Plyr, State, [X, Y], S),
		Score is S + 1, !
	; 
		Square = Plyr,
		Score is 0, !
	;
		fail
	).

moveDir(n, [X, Y], [X, Y1]) :- 
	Y1 is Y - 1.
moveDir(ne, [X, Y], [X1, Y1]) :- 
	X1 is X + 1,
	Y1 is Y - 1.
moveDir(e, [X, Y], [X1, Y]) :- 
	X1 is X + 1.
moveDir(se, [X, Y], [X1, Y1]) :- 
	X1 is X + 1,
	Y1 is Y + 1.
moveDir(s, [X, Y], [X, Y1]) :- 
	Y1 is Y + 1.
moveDir(sw, [X, Y], [X1, Y1]) :- 
	X1 is X - 1,
	Y1 is Y + 1.
moveDir(w, [X, Y], [X1, Y]) :- 
	X1 is X -1.
moveDir(nw, [X, Y], [X1, Y1]) :- 
	X1 is X - 1,
	Y1 is Y - 1.



% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%h(State,Val)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define h(State,Val). 
%   - given State, returns heuristic Val of that state
%   - larger values are good for Max, smaller values are good for Min
%   NOTE1. If State is terminal h should return its true value.
%   NOTE2. If State is not terminal h should be an estimate of
%          the value of state (see handout on ideas about
%          good heuristics.

h(State, Val) :- calcScore(State, 1, S1), calcScore(State, 2, S2), Val is S2 - S1.



% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%lowerBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define lowerBound(B).  
%   - returns a value B that is less than the actual or heuristic value
%     of all states.

lowerBound(-101).




% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%upperBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define upperBound(B). 
%   - returns a value B that is greater than the actual or heuristic value
%     of all states.

upperBound(101).



% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                       %
%                                                                       %
%                Given   UTILITIES                                      %
%                   do NOT change these!                                %
%                                                                       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% get(Board, Point, Element): get the value of the board at position
% column X and row Y (indexing starts at 0).
% Do not change get:

get( Board, [X, Y], Value) :- 
	nth0( Y, Board, ListY), 
	nth0( X, ListY, Value).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% set( Board, NewBoard, [X, Y], Value): set the value of the board at position
% column X and row Y to Value (indexing starts at 0). Returns the new board as
% NewBoard. Do not change set:

set( [Row|RestRows], [NewRow|RestRows], [X, 0], Value) :-
    setInList(Row, NewRow, X, Value). 

set( [Row|RestRows], [Row|NewRestRows], [X, Y], Value) :-
    Y > 0, 
    Y1 is Y-1, 
    set( RestRows, NewRestRows, [X, Y1], Value). 

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% setInList( List, NewList, Index, Value): given helper to set. Do not
% change setInList:

setInList( [_|RestList], [Value|RestList], 0, Value). 

setInList( [Element|RestList], [Element|NewRestList], Index, Value) :- 
	Index > 0, 
	Index1 is Index-1, 
	setInList( RestList, NewRestList, Index1, Value). 
 
%%%%% Helpers %%%%%%%%%
%
%Placed here since they didn't really belong anywhere else
%

% Gets the enemy
isEnemy(1, 2).
isEnemy(2, 1).

% Check if coordinates are on the board
isOnTheBoard([X, Y]) :- between(0, 5, X), between(0, 5, Y).
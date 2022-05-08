% Simon Lundberg

% Insertionsort implementation doesn't work properly 

%ins(X, [], [X]) :- !.
%ins(X1, [X2|L], [X1, X2|L]) :-
%    X1 = sublist(S1,_,_,_),
%    X2 = sublist(S2,_,_,_),
%    S1 =< S2, !.
%ins(X1, [X2|L], [X2|L1]) :- ins(X1, L, L1).
%
%insertionSort([],[]) :- !.
%insertionSort([X,L], S) :- insertionSort(L, S1), ins(X, S1, S).

sorting(I, O) :- sort(1, @=<, I, O). % Using prologs own sort instead

generate([_], []).
generate([X | T], [X | O]):-
    generate(T, O).

sublistsInner([], _, []).
sublistsInner([X | T], I, [[S, I, J, [X | T]] | XS]) :-
    generate([X | T], Gen),
    sum_list([X | T], S),
    sublistsInner(Gen, I, XS),
    length([X | T], L),
    J is I + L - 1.

sublists([], _, []).
sublists([X | T], I, [Inner | XS]) :-
    J is I+1,
    sublistsInner([X | T], I, Inner),
    sublists(T, J, XS).

smallestK(0, _, []).
smallestK(K, [L | LS], [L | XS]) :-
    M is K-1,
    smallestK(M, LS, XS).

toStringInner([S, I, J, L]) :-
    write(S), write('\t'), write(I), write('\t'), write(J), write('\t'), write(L), write('\n').

toString([]).
toString([X | XS]) :-
    toStringInner(X),
    toString(XS).

testTemplate(K, L) :-
    sublists(L, 1, X),
    append(X,Y),
    sorting(Y, Sorted),
    smallestK(K, Sorted, O),
    write("Size, \t Start \t End \t List\n"),
    toString(O).

test0 :- testTemplate(3, [-1, 2, -3, 4, -5]).

% Generate List for test 1

testList(S, S, []).
testList(S, E, [X|L]) :-
    S =< E,
    IterS is S + 1,
    X is S * (-1) ^ S,
    testList(IterS, E, L).

test1 :- testList(1, 100, L), testTemplate(15, L).

test2 :- testTemplate(6, [24, -11, -34, 42, -24, 7, -19, 21]).

test3 :- testTemplate(8, [3, 2, -4, 3, 2, -5, -2, 2, 3, -3, 2, -5, 6, -2, 2, 3]).
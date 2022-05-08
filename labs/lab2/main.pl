% Simon Lundberg

%ins(X, [], [X]) :- !.
%ins(X1, [X2|L], [X1, X2|L]) :-
%    X1 = sublist(S1,_,_,_),
%    X2 = sublist(S2,_,_,_),
%    S1 =< S2, !.
%ins(X1, [X2|L], [X2|L1]) :- ins(X1, L, L1).
%
%insertionSort([],[]) :- !.
%insertionSort([X,L], S) :- insertionSort(L, S1), ins(X, S1, S).

sorting(I, O) :- sort(1, @=<, I, O).

sum(L, S) :- sum_list(L, S).

generate([_], []).
generate([X | T], [X | O]):-
    generate(T, O).

sublistsInner([], _, []).
sublistsInner([X | T], I, [[S, I, J, [X | T]] | XS]) :-
    generate([X | T], Gen),
    sum([X | T], S),
    sublistsInner(Gen, I, XS),
    length([X | T], L),
    J is I + L - 1.


sublists([], _, []).
sublists([X | T], I, [Inner | XS]) :-
    J is I+1,
    sublistsInner([X | T], I, Inner),
    sublists(T, J, XS).

smallestK(0, _, []).
smallestK(K, [List | Lists], [List | Xs]) :-
    L is K-1,
    smallestK(L, Lists, Xs).


toStringInner([S, I, J, X]) :-
    write(S), write('\t'), write(I), write('\t'), write(J), write('\t'), write(L), write('\n').

toString([]).
toString([X | XS]) :-
    toStringInner(X),
    toString(XS).


testTemplate(K, L) :-
    sublists(L, 1, X),
    append(X,Y),
    sorting(Y, Sorted),
    smallestK(K, Sorted, Out),
    write("Size, \t Start \t End \t List\n"),
    toString(Out).

test1 :- testTemplate(3, [-1, 2, -3, 4, -5]).


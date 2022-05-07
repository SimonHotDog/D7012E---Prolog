ins(X, [], [X]) :- !.
ins(
    sublist(S1, I1, J1, L1),
    [sublist(S2, I2, J2, L2)|T],
    [sublist(S1, I1, J1, L1), sublist(S2, I2, J2, L2)|T]) :- S1 =< S2, !.
ins(X, [X1|T1], [X2,T2]) :- ins(X, T1, T2).

insertionSort([],[]).
insertionSort([X,T], T2) :- insertionSort(T, T1), ins(X, T1, T2).

sum(L, S) :- sum_list(L, S).

% Sublist generation

generate([_], []).
generate([H | T], [H | O]) :- generate(T, O).

sublistsInner([], _, []).
sublistsInner([In, T], I, [[S, I, J, [In | T]] | XS]) :-
    generate([In | T], Gen),
    sum([In | T], S),
    sublistsInner(Gen, I, XS),
    length([In | T], L),
    J is I + L - 1.

sublists([], _, []).
sublists([In | T], I, [Inner | XS]) :-
    J is I + 1,
    sublistsInner([In | T], I, Inner),
    sublists(T, J, XS).

% Final part

smallestK(0, _, []).
smallestK(K, [L | LS], [L | XS]) :-
    L is K-1,
    smallestK(L, LS, XS).

% Printing mess

toStringLine([S1, I, J, X], Out) :-
    string_concat(S1, "\t", S2),
    string_concat(S2, I, S3),
    string_concat(S3, "\t", S4),
    string_concat(S4, J, S5),
    string_concat(S5, "\t", S6),
    atomics_to_string(X, ",", SX),
    string_concat(S6, SX, S7),
    string_concat(S7, "/n", Out).

toString([]).
toString([X | XS]) :-
    toStringLine(X, Out),
    write(Out),
    toString(XS).

testTemplate(K, L) :-
    sublists(L, 1, X),
    append(X, Y),
    insertionSort(Y, Sorted),
    smallestK(K, Sorted, Out),
    write("Size, \t Start \t End \t List\n"),
    toString(Out).

test1 :- testTemplate(3, [-1, 2, -3, 4, -5]).


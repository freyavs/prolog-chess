:- module(utilities, [add_if_needed/3, between_ex/3, between_in/3]).

% ------- GENERAL UTILITIES --------

% add_if_needed(List, OtherList, ResultList)
% same as append, but only append an element of List to OtherList if element is not in OtherList
% ResultList contains the concatenation of List (not necessarily all elements of List) and OtherList
add_if_needed([], R, R) :- !.
add_if_needed([X | L], R, Result) :- member(X, R), !, add_if_needed(L, R, Result).
add_if_needed([X | L], R, [ X | Result]) :- add_if_needed(L, R, Result). 


% between_in(Number1, Number2, Number3)
% sets Number3 to a number between Number1 and Number2, including Number1 and Number2
between_in(N, M, R) :- between(N,M,R).
between_in(N, M, R) :- between(M,N,R).

% between_ex(Number1, Number2, Number3)
% sets Number3 to a number between Number1 and Number2, excluding Number1 and Number2
between_ex(N, M, R) :- between(N,M,R), R > N, R < M.
between_ex(N, M, R) :- between(M,N,R), N > R, M < R.

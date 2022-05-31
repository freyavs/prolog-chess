:- begin_tests(board).
:- use_module('../src/chess_board').

% test opponent predicate
test(opponent) :- 
    opponent(white, black), 
    \+ opponent(black, black),
    opponent(black, white).

% test contains predicate
test(contains) :- 
    init_board(B),
    contains(B, black, 2/8),
    \+ contains(B, white, 2/8),
    contains(B, white, 1/2),
    \+ contains(B, black, 1/2).

% test update_move predicate
test(update_move) :-
    init_board(B),
    member(piece(white, pawn, 3/2), B),
    update_move(B, piece(white, pawn, 3/2),4/2, R),
    contains(R, white, 4/2),
    \+ contains(R, white, 3/2),
    member(piece(white, pawn, 4/2), R),
    \+ member(piece(white, pawn, 3/2), R),
    length(R, 32).

% vertical and horizontal piece_between tests
test(piece_between1) :- 
    init_board(B),
    piece_between(B, 1/1, 1/4).

test(piece_between2) :- 
    init_board(B),
    piece_between(B, 6/8, 6/3).

test(no_piece_between3) :- 
    init_board(B),
    \+ piece_between(B, 2/2, 2/3).

test(piece_between4) :- 
    init_board(B),
    \+ piece_between(B, 2/2, 3/2).

test(piece_between5) :- 
    init_board(B),
    \+ piece_between(B, 4/4, 4/6).

% diagonal piece_between tests
test(piece_between6) :- 
    init_board(B), 
    \+ piece_between(B, 1/6,  4/3).

test(piece_between7) :- 
    init_board(B), 
    piece_between(B, 1/8,  4/5).

test(piece_between8) :- 
    init_board(B), 
    piece_between(B, 1/1,  3/3).


:- end_tests(board).
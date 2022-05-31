
:- begin_tests(rook).
:- use_module('../src/chess_board').
:- use_module('../src/piece_moves').


% test all valid moves for rook in start position without pawn in front of it
test(move1_white) :- 
    init_board(B),
    Piece = piece(white, rook, 8/1),
    select(piece(white, pawn, 8/2), B, Board),
    findall(X/Y, possible_move(Piece, X/Y), Moves),
    length(Moves, 14),
    findall(N/M, move(Board, Piece, N/M, _), ValidMoves),
    length(ValidMoves, 6).

% moving a rook from its initial position should cause castle blocking logic and update board correctly
test(move2_white) :- 
    init_board(B),
    Piece = piece(white, rook, 8/1),
    select(piece(white, pawn, 8/2), B, Board),
    move(Board, Piece, 8/3, Result),
    member(castle_blocker(white, 7/1), Result),
    member(piece(white, rook, 8/3), Result),
    \+ member(Piece, Result).


% rook should be able to take piece at the other side of the board
test(move3_white) :- 
    init_board(B),
    Piece = piece(white, rook, 8/1),
    select(piece(white, pawn, 8/2), B, Board),
    move(Board, Piece, 8/7, Result),
    member(castle_blocker(white, 7/1), Result),
    member(piece(white, rook, 8/7), Result),
    \+ member(Piece, Result),
    \+ member(piece(black, pawn, 8/7), Result).


% rook should not be able to move in initial position
test(move4_white) :- 
    init_board(B),
    Piece = piece(white, rook, 8/1),
    findall(N/M, move(B, Piece, N/M, _), ValidMoves),
    length(ValidMoves, 0).


:- end_tests(rook).
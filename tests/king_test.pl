:- begin_tests(king).
:- use_module('../src/chess_board').
:- use_module('../src/piece_moves').


% test all valid moves for king in start position without 2 pawns in front of it
test(move1_black) :- 
    init_board(B),
    Piece = piece(black, king, 5/8),
    select(piece(black, pawn, 5/7), B, Board),
    select(piece(black, pawn, 6/7), Board, Board2),
    findall(X/Y, possible_move(Piece, X/Y), Moves),
    length(Moves, 8),
    findall(N/M, move(Board2, Piece, N/M, _), ValidMoves),
    length(ValidMoves, 2).

% black king with white pawn near should be able to take it, and move sideways if queen is not there
test(move2_black) :- 
    init_board(B),
    Piece = piece(black, king, 5/8),
    select(piece(black, pawn, 6/7), B, Board),
    select(piece(black, queen, 4/8), Board, Board2),
    NewPiece = piece(white, pawn, 6/7),
    Board3 = [ NewPiece | Board2 ],
    findall(N/M, move(Board3, Piece, N/M, _), ValidMoves),
    length(ValidMoves, 2),
    member(4/8, ValidMoves),
    member(6/7, ValidMoves).

% king taking a piece should work correctly, and add castle_blockers when moving from initial position
test(move3_black) :- 
    init_board(B),
    Piece = piece(black, king, 5/8),
    select(piece(black, pawn, 6/7), B, Board),
    NewPiece = piece(white, pawn, 6/7),
    Board2 = [ NewPiece | Board ],
    move(Board2, Piece, 6/7, Result),
    member(piece(black, king, 6/7), Result),
    \+ member(piece(white, pawn, 6/7), Result),
    member(castle_blocker(black, 7/8), Result),
    member(castle_blocker(black, 3/8), Result).

% king should not be able to move in initial position
test(move4_black) :- 
    init_board(B),
    Piece = piece(black, king, 5/8),
    findall(N/M, move(B, Piece, N/M, _), ValidMoves),
    length(ValidMoves, 0).

% black should be able to castle king's side when there are no pieces between
test(move5_black) :-
    init_board(B),
    Piece = piece(black, king, 5/8),
    select(piece(black, bishop, 6/8), B, Board),
    select(piece(black, knight, 7/8), Board, Board2),
    findall(N/M, move(Board2, Piece, N/M, _), ValidMoves),
    length(ValidMoves, 2),
    move(Board2, Piece, 7/8, Result),
    member(piece(black, king, 7/8), Result),
    member(piece(black, rook, 6/8), Result),
    member(castle_blocker(black, 7/8), Result),
    member(castle_blocker(black, 3/8), Result).

% black should be able to castle queen's side when there are no pieces between
test(move6_black) :-
    init_board(B),
    Piece = piece(black, king, 5/8),
    select(piece(black, bishop, 3/8), B, Board),
    select(piece(black, knight, 2/8), Board, Board2),
    select(piece(black, queen, 4/8), Board2, Board3),
    findall(N/M, move(Board3, Piece, N/M, _), ValidMoves),
    length(ValidMoves, 2),
    move(Board3, Piece, 3/8, Result),
    member(piece(black, king, 3/8), Result),
    member(piece(black, rook, 4/8), Result),
    member(castle_blocker(black, 7/8), Result),
    member(castle_blocker(black, 3/8), Result).


% black should not be able to castle if king is under attack
test(move7_black) :-
    init_board(B),
    Piece = piece(black, king, 5/8),
    select(piece(black, bishop, 6/8), B, Board),
    select(piece(black, knight, 7/8), Board, Board2),
    select(piece(black, pawn, 5/7), Board2, Board3),
    Board4 = [ piece(white, queen, 5/3) | Board3 ], 
    findall(N/M, move(Board4, Piece, N/M, _), ValidMoves),
    length(ValidMoves, 1),
    \+ member(7/8, ValidMoves),
    member(6/8, ValidMoves).

% black should be able to castle if king or castling squares are not under attack
test(move8_black) :-
    init_board(B),
    Piece = piece(black, king, 5/8),
    select(piece(black, bishop, 6/8), B, Board),
    select(piece(black, knight, 7/8), Board, Board2),
    select(piece(black, pawn, 5/7), Board2, Board3),
    select(piece(black, pawn, 6/7), Board3, Board4),
    select(piece(black, pawn, 7/7), Board4, Board5),
    select(piece(black, pawn, 8/7), Board5, Board6),
    findall(N/M, move(Board6, Piece, N/M, _), ValidMoves),
    length(ValidMoves, 4),
    member(7/8, ValidMoves).

% black should not be able to castle if castling square is under attack
test(move9_black) :-
    init_board(B),
    Piece = piece(black, king, 5/8),
    select(piece(black, bishop, 6/8), B, Board),
    select(piece(black, knight, 7/8), Board, Board2),
    select(piece(black, pawn, 5/7), Board2, Board3),
    select(piece(black, pawn, 6/7), Board3, Board4),
    select(piece(black, pawn, 7/7), Board4, Board5),
    select(piece(black, pawn, 8/7), Board5, Board6),
    Board7 = [ piece(white, bishop, 3/5) | Board6],
    findall(N/M, move(Board7, Piece, N/M, _), ValidMoves),
    length(ValidMoves, 1),
    member(6/7, ValidMoves).

% black should not be able to castle if castling square is under attack (first test)
test(move10_black) :-
    init_board(B),
    Piece = piece(black, king, 5/8),
    select(piece(black, bishop, 6/8), B, Board),
    select(piece(black, knight, 7/8), Board, Board2),
    select(piece(black, pawn, 5/7), Board2, Board3),
    select(piece(black, pawn, 6/7), Board3, Board4),
    select(piece(black, pawn, 7/7), Board4, Board5),
    select(piece(black, pawn, 8/7), Board5, Board6),
    Board7 = [ piece(white, rook, 7/6) | Board6],
    findall(N/M, move(Board7, Piece, N/M, _), ValidMoves),
    length(ValidMoves, 3),
    \+ member(7/8, ValidMoves).

% black should be able to castle if castling square is not under attack (second test)
test(move10_black) :-
    init_board(B),
    Piece = piece(black, king, 5/8),
    select(piece(black, bishop, 6/8), B, Board),
    select(piece(black, knight, 7/8), Board, Board2),
    select(piece(black, pawn, 5/7), Board2, Board3),
    select(piece(black, pawn, 6/7), Board3, Board4),
    select(piece(black, pawn, 7/7), Board4, Board5),
    select(piece(black, pawn, 8/7), Board5, Board6),
    Board7 = [ piece(white, rook, 8/7) | Board6],
    findall(N/M, move(Board7, Piece, N/M, _), ValidMoves),
    length(ValidMoves, 2),
    member(7/8, ValidMoves),
    member(6/8, ValidMoves).

:- end_tests(king).
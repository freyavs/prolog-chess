:- begin_tests(pawns).
:- use_module('../src/chess_board').
:- use_module('../src/piece_moves').


% test all start moves for a white pawn (can move 1 or 2 forward)
test(move1_white) :- 
    init_board(B),
    Piece = piece(white, pawn, 2/2),
    findall(X/Y, possible_move(Piece, X/Y), Moves),
    length(Moves, 2),
    member(2/3, Moves),
    member(2/4, Moves),
    move(B, Piece, 2/3, _),
    move(B, Piece, 2/4, _).

% test all start moves where a white pawn would take, which should not possible at the very beginning
test(move2_white) :- 
    init_board(B),
    Piece = piece(white, pawn, 2/2),
    findall(X/Y, possible_take(Piece, X/Y), Moves),
    length(Moves, 2),
    member(1/3, Moves),
    member(3/3, Moves),
    \+ move(B, Piece, 1/2, _),
    \+ move(B, Piece, 3/2, _).

% pawn should not be able to move if a piece is in front of it 
test(move3_white) :- 
    init_board(Board),
    B = [ piece(black, knight, 5/3) | Board ], % add random piece in front of pawn
    Piece = piece(white, pawn, 5/2),
    findall(X/Y, move(B, Piece, X/Y, _), Moves),
    length(Moves, 0).

% pawn should be able to move only one square if a piece is in front of the second square (if pawn has not moved yet)
test(move4_white) :- 
    init_board(Board),
    B = [ piece(black, knight, 5/4) | Board ], % add random piece in front of pawn
    Piece = piece(white, pawn, 5/2),
    findall(X/Y, move(B, Piece, X/Y, _), Moves),
    length(Moves, 1),
    member(5/3, Moves).

% pawn should only be able to make one move - which is take the piece -, if a piece is in front of it and diagonally in front of it
test(move5_white) :- 
    init_board(Board),
    NewPiece = piece(black, pawn, 4/3),
    B = [ piece(white, knight, 5/3), NewPiece  | Board ], % add random pieces in front of pawn
    Piece = piece(white, pawn, 5/2),
    findall(X/Y, move(B, Piece, X/Y, _), Moves),
    length(Moves, 1),
    member(4/3, Moves),
    move(B, Piece, 4/3, R),
    \+ member(NewPiece, R).

% pawn should be able to do all moves it can possibly do if it's able to take either a piece left or right
test(move6_white) :- 
    init_board(Board),
    B = [ piece(black, knight, 5/3), piece(black, knight, 7/3)  | Board ], 
    Piece = piece(white, pawn, 6/2),
    findall(X/Y, move(B, Piece, X/Y, _), Moves),
    findall(X/Y, possible_move(Piece, X/Y), AllMoves),
    findall(X/Y, possible_take(Piece, X/Y), AllTakes),
    length(AllMoves, N),
    length(AllTakes, M),
    length(Moves, S),
    TotalMoves is N + M,
    S = TotalMoves.

% pawn should not be able to take a piece of the same colour 
test(move7_white) :- 
    init_board(Board),
    NewPiece = piece(white, pawn, 4/3),
    B = [ piece(black, knight, 5/3), NewPiece  | Board ], % add random pieces in front of pawn
    Piece = piece(white, pawn, 5/2),
    findall(X/Y, move(B, Piece, X/Y, _), Moves),
    length(Moves, 0).


% test all valid start moves for a black pawn (can move 1 or 2 forward)
test(move1_black) :- 
    init_board(B),
    Piece = piece(black, pawn, 4/7),
    findall(X/Y, possible_move(Piece, X/Y), Moves),
    length(Moves, 2),
    member(4/6, Moves),
    member(4/5, Moves),
    move(B, Piece, 4/6, _),
    move(B, Piece, 4/5, _).


% test all start moves where a black pawn would take, which should not possible at the very beginning
test(move2_black) :- 
    init_board(B),
    Piece = piece(black, pawn, 4/7),
    findall(X/Y, possible_take(Piece, X/Y), Moves),
    length(Moves, 2),
    member(3/6, Moves),
    member(5/6, Moves),
    \+ move(B, Piece, 3/6, _),
    \+ move(B, Piece, 5/6, _).

% pawn should not be able to move if a piece is in front of it 
test(move3_black) :- 
    init_board(Board),
    B = [ piece(white, knight, 5/2) | Board ], % add random piece in front of pawn
    Piece = piece(white, pawn, 5/3),
    findall(X/Y, move(B, Piece, X/Y, _), Moves),
    length(Moves, 0).

% pawn should be able to move only one square if a piece is in front of the second square (if pawn has not moved yet)
test(move4_black) :- 
    init_board(Board),
    B = [ piece(black, knight, 5/5) | Board ], % add random piece in front of pawn
    Piece = piece(black, pawn, 5/7),
    findall(X/Y, move(B, Piece, X/Y, _), Moves),
    length(Moves, 1),
    member(5/6, Moves).

% pawn should only be able to make one move - which is take the piece -, if a piece is in front of it and diagonally in front of it
test(move5_black) :- 
    init_board(Board),
    NewPiece = piece(white, pawn, 4/6),
    B = [ piece(white, knight, 5/6), NewPiece  | Board ], % add random pieces in front of pawn
    Piece = piece(black, pawn, 5/7),
    findall(X/Y, move(B, Piece, X/Y, _), Moves),
    length(Moves, 1),
    member(4/6, Moves),
    move(B, Piece, 4/6, R),
    \+ member(NewPiece, R).

% pawn should be able to do all moves it can possibly do if it's able to take either a piece left or right
test(move6_black) :- 
    init_board(Board),
    B = [ piece(white, knight, 5/6), piece(white, knight, 7/6)  | Board ], 
    Piece = piece(black, pawn, 6/7),
    findall(X/Y, move(B, Piece, X/Y, _), Moves),
    findall(X/Y, possible_move(Piece, X/Y), AllMoves),
    findall(X/Y, possible_take(Piece, X/Y), AllTakes),
    length(AllMoves, N),
    length(AllTakes, M),
    length(Moves, S),
    TotalMoves is N + M,
    S = TotalMoves.

% pawn should not be able to take a piece of the same colour 
test(move7_black) :- 
    init_board(Board),
    NewPiece = piece(black, pawn, 4/2),
    B = [ piece(black, knight, 5/2), NewPiece  | Board ], % add random pieces in front of pawn
    Piece = piece(black, pawn, 5/3),
    findall(X/Y, move(B, Piece, X/Y, _), Moves),
    length(Moves, 0).


% ------------------- en passant tests -------------------------

% moving pawn 2 squares should cause en_passant to appear on board, moving opponent piece should remove the en_passant
test(move_white_enpassant1) :-
    init_board(Board),
    move(Board, piece(white, pawn, 3/2), 3/4, Board2),
    member(en_passant(white, 3/3), Board2),
    move(Board2, piece(black, pawn, 3/7), 3/6, Board3),
    \+ member(en_passant(white, 3/3), Board3).

test(move_black_enpassant1) :-
    init_board(Board),
    move(Board, piece(black, pawn, 8/7), 8/5, Board2),
    member(en_passant(black, 8/6), Board2),
    move(Board2, piece(white, pawn, 8/2), 8/3, Board3),
    \+ member(en_passant(black, 8/6), Board3).

% moving a pawn 3 times to the front should not cause any en passant logic
test(move_white_enpassant2) :-
    init_board(Board),
    move(Board, piece(white, pawn, 3/2), 3/3, Board2),
    move(Board2, piece(white, pawn, 3/3), 3/4, Board3),
    \+ member(en_passant(white, 3/3), Board3),
    move(Board3, piece(white, pawn, 3/4), 3/5, Board4),
    \+ member(en_passant(white, 3/3), Board4).

test(move_black_enpassant2) :-
    init_board(Board),
    move(Board, piece(black, pawn, 8/7), 8/6, Board2),
    move(Board2, piece(black, pawn, 8/6), 8/5, Board3),
    \+ member(en_passant(black, 8/6), Board3),
    move(Board3, piece(black, pawn, 8/5), 8/4, Board4),
    \+ member(en_passant(black, 8/6), Board4).

% pawn should be able to take en passant, removing en_passant and the pawn
test(move_white_enpassant3) :-
    init_board(Board),
    move(Board, piece(white, pawn, 3/2), 3/4, Board2),
    Pawn = piece(black, pawn, 2/4),
    Board3 = [ Pawn | Board2 ],
    findall(X/Y, move(Board3, Pawn, X/Y, _), Moves),
    length(Moves, 2),
    member(3/3, Moves),
    move(Board3, Pawn, 3/3, Board4),
    member(piece(black, pawn, 3/3), Board4),
    \+ member(piece(white, pawn, 3/4), Board4),
    \+ member(en_passant(white, 3/3), Board4).

% pawn should be able to take en passant, removing en_passant and the pawn
test(move_black_enpassant3) :-
    init_board(Board),
    move(Board, piece(black, pawn, 3/7), 3/5, Board2),
    Pawn = piece(white, pawn, 2/5),
    Board3 = [ Pawn | Board2 ],
    findall(X/Y, move(Board3, Pawn, X/Y, _), Moves),
    length(Moves, 2),
    member(3/6, Moves),
    move(Board3, Pawn, 3/6, Board4),
    member(piece(white, pawn, 3/6), Board4),
    \+ member(piece(black, pawn, 3/5), Board4),
    \+ member(en_passant(black, 3/6), Board4).

% pawn en passant should be removed after it's not taken 
test(move_black_enpassant4) :-
    init_board(Board),
    move(Board, piece(black, pawn, 3/7), 3/5, Board2),
    Pawn = piece(white, pawn, 2/2),
    Board3 = [ Pawn | Board2 ],
    move(Board3, Pawn, 2/3, Board4),
    member(piece(white, pawn, 2/3), Board4),
    member(piece(black, pawn, 3/5), Board4),
    \+ member(en_passant(black, 3/6), Board4).

% pawn reaching other side should be able to be replaced
test(move_white_end) :-
    Piece = piece(white, pawn, 3/7),
    Board = [ Piece , piece(black, king, 8/8), piece(white, king, 8/1)],
    findall( Result, move(Board, Piece, 3/8, Result), Results),
    length(Results, 4),
    move(Board, Piece, 3/8, Result),
    \+ member(piece(white, pawn, 3/8), Result).


% pawn reaching other side should be able to be replaced
test(move_black_end) :-
    Piece = piece(black, pawn, 1/2),
    Board = [ Piece , piece(black, king, 8/8), piece(white, king, 8/1)],
    findall( Result, move(Board, Piece, 1/1, Result), Results),
    length(Results, 4),
    move(Board, Piece, 1/1, Result),
    \+ member(piece(black, pawn, 1/1), Result).

:- end_tests(pawns).
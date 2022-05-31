:- begin_tests(knight).
:- use_module('../src/chess_board').
:- use_module('../src/piece_moves').


% test all valid moves for knight in start position 
test(move1_white) :- 
    init_board(B),
    Piece = piece(white, knight, 2/1),
    findall(X/Y, possible_move(Piece, X/Y), Moves),
    length(Moves, 8),
    findall(N/M, move(B, Piece, N/M, _), ValidMoves),
    length(ValidMoves, 2),
    member(1/3, ValidMoves),
    member(3/3, ValidMoves).

% knight in middle of board should be able to take white pawns but not black pawns 
test(move2_white) :- 
    init_board(B),
    Piece = piece(white, knight, 5/5),
    Board = [ Piece | B ],
    findall(N/M, move(Board, Piece, N/M, _), ValidMoves),
    length(ValidMoves, 8),
    member(4/7, ValidMoves),
    member(6/7, ValidMoves),
    member(3/4, ValidMoves),
    member(7/4, ValidMoves).

% knight taking a piece should work correctly
test(move2_white) :- 
    init_board(B),
    Piece = piece(white, knight, 5/5),
    Board = [ Piece | B ],
    move(Board, Piece, 4/7, Result),
    member(piece(white, knight, 4/7), Result),
    \+ member(piece(black, pawn, 4/7), Result).


:- end_tests(knight).

:- begin_tests(queen).
:- use_module('../src/chess_board').
:- use_module('../src/piece_moves').


% test all valid moves for queen in start position without pawn in front of it
test(move1_black) :- 
    init_board(B),
    Piece = piece(black, queen, 4/8),
    select(piece(black, pawn, 4/7), B, Board),
    findall(X/Y, possible_move(Piece, X/Y), Moves),
    length(Moves, 21),
    findall(N/M, move(Board, Piece, N/M, _), ValidMoves),
    length(ValidMoves, 6).

% queen in middle of board should be able to do all its moves and to take white pawns but not black pawns 
test(move2_black) :- 
    init_board(B),
    Piece = piece(black, queen, 4/6),
    Board = [ Piece | B ],
    findall(N/M, move(Board, Piece, N/M, _), ValidMoves),
    length(ValidMoves, 18).

% queen taking a piece should work correctly
test(move2_black) :- 
    init_board(B),
    Piece = piece(black, queen, 4/6),
    Board = [ Piece | B ],
    move(Board, Piece, 4/3, Result),
    member(piece(black, queen, 4/3), Result),
    \+ member(piece(white, pawn, 4/3), Result).

% queen should not be able to move in initial position
test(move4_black) :- 
    init_board(B),
    Piece = piece(black, queen, 4/8),
    findall(N/M, move(B, Piece, N/M, _), ValidMoves),
    length(ValidMoves, 0).

:- end_tests(queen).
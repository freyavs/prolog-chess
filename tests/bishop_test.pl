:- begin_tests(bishop).
:- use_module('../src/chess_board').
:- use_module('../src/piece_moves').


% test all valid moves for bishop in start position without pawn in front (to the side) of it
test(move1_black) :- 
    init_board(B),
    Piece = piece(black, bishop, 3/8),
    select(piece(black, pawn, 4/7), B, Board),
    findall(X/Y, possible_move(Piece, X/Y), Moves),
    length(Moves, 7),
    findall(N/M, move(Board, Piece, N/M, _), ValidMoves),
    length(ValidMoves, 5).

% bishop in middle of board should be able to do all its moves and take white pieces but not black pieces 
test(move2_black) :- 
    init_board(B),
    Piece = piece(black, bishop, 4/5),
    Board = [ Piece | B ],
    findall(N/M, move(Board, Piece, N/M, _), ValidMoves),
    length(ValidMoves, 8),
    member(1/2, ValidMoves),
    member(7/2, ValidMoves),
    member(5/6, ValidMoves),
    \+ member(2/7, ValidMoves).

% bishop taking a piece should work correctly
test(move3_black) :- 
    init_board(B),
    Piece = piece(black, bishop, 4/5),
    Board = [ Piece | B ],
    move(Board, Piece, 7/2, Result),
    member(piece(black, bishop, 7/2), Result),
    \+ member(piece(white, pawn, 7/2), Result).

% bishop should not be able to move in initial position
test(move4_black) :- 
    init_board(B),
    Piece = piece(black, bishop, 3/8),
    findall(N/M, move(B, Piece, N/M, _), ValidMoves),
    length(ValidMoves, 0).


:- end_tests(bishop).
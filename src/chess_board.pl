:- module(chess_board, 
    [   init_board/1,       
        opponent/2,         
        move/4,             
        piece_between/3, 
        update_move/4,   
        contains/3,
        valid_move/3,
        get_blocker/3,
        castle_side/2,
        color_row/2,
        is_in_checkmate/2,
        is_in_stalemate/2,
        get_result/3,
        get_all_moves/3,
        init_piece/1
    ]).

:- use_module(piece_moves).
:- use_module(utilities).

% --------------------------- initialisation methods --------------------------------------

% init_piece(Piece)
% Piece is a piece on an initial chess board, where no pieces have moved yet 
init_piece(piece(white, rook,   1/1)).
init_piece(piece(white, knight, 2/1)).
init_piece(piece(white, bishop, 3/1)).
init_piece(piece(white, queen,  4/1)).
init_piece(piece(white, king,   5/1)).
init_piece(piece(white, bishop, 6/1)).
init_piece(piece(white, knight, 7/1)).
init_piece(piece(white, rook,   8/1)).
init_piece(piece(white, pawn,   X/2)) :-
    between(1, 8, X).

init_piece(piece(black, rook,   1/8)).
init_piece(piece(black, knight, 2/8)).
init_piece(piece(black, bishop, 3/8)).
init_piece(piece(black, queen,  4/8)).
init_piece(piece(black, king,   5/8)).
init_piece(piece(black, bishop, 6/8)).
init_piece(piece(black, knight, 7/8)).
init_piece(piece(black, rook,   8/8)).
init_piece(piece(black, pawn,   X/7)) :-
    between(1, 8, X).

% init_board(Board)
% Board represents an initial chess board, where pieces have not moved yet
init_board(Board) :- findall(P, init_piece(P), Board).

% -------------------------------------- move control/check methods ---------------------------------------

% inside_board(X/Y)
% True if X (the column) and Y (the row) are inside the chess board
inside_board(X/Y) :-
    X < 9,
    X > 0,
    Y < 9,
    Y > 0.

% piece_between(Board, Position, OtherPosition)
% Board contains the current pieces on the chess board
% True if there is a piece between Position and OtherPosition when Position and OtherPosition are on the same column
piece_between(Board, X/Y, X/B) :-
    !,
    between_ex(Y, B, TestY),
    contains(Board, _, X/TestY),
    !.

% piece_between(Board, Position, OtherPosition)
% Board contains the current pieces on the chess board
% True if there is a piece between Position and OtherPosition when Position and OtherPosition are on the same row
piece_between(Board, X/Y, A/Y) :-
    !,
    between_ex(X, A, TestX),
    contains(Board, _, TestX/Y),
    !.

% piece_between(Board, Position, OtherPosition)
% Board contains the current pieces on the chess board
% True if there is a piece between Position and OtherPosition when Position and OtherPosition are on the same diagonal 
piece_between(Board, X/Y, A/B) :-
    from_to(X,A,TestX),
    from_to(Y,B,TestY),
    (contains(Board, _, TestX/TestY) -> true 
    ;
    piece_between(Board, TestX/TestY, A/B)). 

from_to(X, A, Test) :- A < X, Test is X-1, A \= Test.
from_to(X, A, Test) :- A > X, Test is X+1, A \= Test.

from_to(Y, B, Test) :- B < Y, Test is Y-1, B \= Test.
from_to(Y, B, Test) :- B > Y, Test is Y+1, B \= Test.

% contains(Board, Color, Position)
% True if Board contains a piece having color Color on Position
contains(Board, Color, X/Y) :- member(piece(Color, _, X/Y), Board).

% opponent(Color, OtherColor)
% True if Color is the opponent of OtherColor
opponent(white, black).
opponent(black, white).

% valid_move(Board, Piece, Position)
% True if moving Piece of type pawn to Position is allowed on the chess board represented by Board
valid_move(Board, piece(Color, pawn, OldPosition), NewPosition) :-
    possible_move(piece(Color, pawn, OldPosition), NewPosition),
    !, % no backtracking when matching on pawn moving forward
    inside_board(NewPosition),
    \+ contains(Board, _, NewPosition),
    \+ piece_between(Board, OldPosition, NewPosition).

% valid_move(Board, Piece, Position)
% True if moving Piece of type knight to Position is allowed on the chess board represented by Board
valid_move(Board, piece(Color, knight, _), NewPosition) :-
    !, % no backtracking when matching on knight
    inside_board(NewPosition),
    \+ contains(Board, Color, NewPosition). % Only opposite colour can be taken 

% valid_move(Board, Piece, Position)
% True if moving Piece to Position is allowed on the chess board represented by Board
valid_move(Board, piece(Color, _, OldPosition), NewPosition) :-
    inside_board(NewPosition),
    \+ contains(Board, Color, NewPosition), % Only opposite colour can be taken 
    \+ piece_between(Board, OldPosition, NewPosition).

% valid_castle(Board, KingPiece, RookPiece, Position)
% True if given a Board, KingPiece can castle to Position, and RookPiece can also reach its castling position
valid_castle(Board, piece(Color, king, KingPosition), piece(Color, rook, RookPosition), NewPosition) :-
    \+ member(castle_blocker(Color, NewPosition), Board),
    \+ piece_between(Board, KingPosition, RookPosition),
    \+ is_threatened_between(Board, Color, KingPosition, NewPosition).

% king_is_check(Board, Color)
% True if given a Board, king of Color is checked
king_is_check(Board, Color) :-
    member(piece(Color, king, X/Y), Board),
    is_threatened(Board, piece(Color, king, X/Y)).

% is_threatened(Board, Piece)
% True if a Piece can be taken by the opposite color given a Board
is_threatened(Board, piece(Color, _, Position)) :-
    opponent(Color, Opponent),
    member(piece(Opponent, OPiece, OPosition), Board),
    possible_take(piece(Opponent, OPiece, OPosition), Position),
    valid_move(Board, piece(Opponent, OPiece, OPosition), Position), !.

% is_threatened_between(Board, Color, Position, OtherPosition)
% True if given a Board and Color, either Position or OtherPosition, or some position in between, is threatend
% helper method for castling, so should only check positions on the same row
is_threatened_between(Board, Color, X/Y, A/Y) :-
    between_in(X, A, R),
    TestPiece = piece(Color, pawn, R/Y),
    is_threatened(Board, TestPiece), !.

% is_in_checkmate(Board, Color)
% True if player of Color is checkmated by opponent given a Board
is_in_checkmate(Board, Color) :-
    king_is_check(Board, Color),
    findnsols(1, R, get_result(Board, Color, R), ResultBoards), !,
    \+ length(ResultBoards, 1).

% is_in_stalemate(Board, Color)
% True if player of Color is stalemated by opponent given a Board
is_in_stalemate(Board, Color) :-
    \+ king_is_check(Board, Color),
    findnsols(1, R, get_result(Board, Color, R), ResultBoards), !,
    \+ length(ResultBoards, 1).

% get_all_moves(Board, Player, ResultBoards)
% sets ResultBoards to list of all possible Boards given which Player should make a move on Board 
get_all_moves(Board, Player, Results) :-
    findall(R, get_result(Board, Player, R), Results).

% get_result(Board, Player, ResultBoard)
% sets ResultBoard to a possible Board given which Player should make a move on Board 
get_result(Board, Player, Result) :-
    member(piece(Player, P, X/Y), Board),
    move(Board, piece(Player, P, X/Y), _, Result).

% add_castling_blockers(Board, Color, Position, ResultBoard)
% sets ResultBoard to Board with castle_blocker for a given Color
% only add castle_blocker if Position is an initial rook position
add_castling_blockers(Board, Color, X/Y, Result) :-
    init_piece(piece(Color, rook,X/Y)),!,
    blocked_column(X, BlockedX),
    Blocker = castle_blocker(Color, BlockedX/Y),
    add_if_needed([Blocker], Board, Result).

add_castling_blockers(Board,_,_,Board).

% -------------------------------------------- UPDATE THE BOARD BASED ON A MOVE ------------------------------------------

% update_move(Board, Piece, NewPosition, ResultBoard)
% update the Board to ResultBoard, based on a move of Piece to NewPosition
% here: pawn skipping a square generates an en_passant piece on the board 
update_move(Board, piece(Color, pawn, X/Y), X/B, ResultBoard) :- 
    init_piece(piece(Color,pawn,X/Y)),
    R is B - Y, Test is abs(R),
    Test = 2, !,
    between_ex(Y, B, EnPassantY),
    select(piece(Color, pawn, X/Y), Board, Board2), % remove old position from the board
    ResultBoard = [piece(Color, pawn, X/B), en_passant(Color, X/EnPassantY) | Board2 ]. % add the new position

% update_move(Board, Piece, NewPosition, ResultBoard)
% update the Board to ResultBoard, based on a move of Piece to NewPosition
% here: if pawn reaches row of opposite color, it can turn into another piece
update_move(Board, piece(Color, pawn, X/Y), A/B, ResultBoard) :- 
    opponent(Color, Opponent),
    color_row(Opponent, Row),
    Row = B, !,
    trade_pawn(NewPiece),
    select(piece(Color, pawn, X/Y), Board, Board2), % remove old position from the board
    ResultBoard = [piece(Color, NewPiece, A/B) | Board2 ]. % add the new position

% update_move(Board, Piece, NewPosition, ResultBoard)
% update the Board to ResultBoard, based on a move of Piece to NewPosition
% here: if king moves from its initial position, add castle blockers on the castling squares (if needed)
update_move(Board, piece(Color, king, X/Y), A/B, ResultBoard) :- 
    init_piece(piece(Color,king,X/Y)), !,
    findall(Blocker, get_blocker(Color, _ , Blocker), Blockers),
    select(piece(Color, king, X/Y), Board, Board2),
    add_if_needed(Blockers, Board2, Board3),
    ResultBoard = [piece(Color, king, A/B) | Board3 ].

% update_move(Board, Piece, NewPosition, ResultBoard)
% update the Board to ResultBoard, based on a move of Piece to NewPosition
% here: if rook moves from its initial position, add castle blockers on the castling square close to it (if needed)
update_move(Board, piece(Color, rook, X/Y), A/B, ResultBoard) :- 
    add_castling_blockers(Board, Color, X/Y, Board2),!,
    select(piece(Color, rook, X/Y), Board2,Board3),
    ResultBoard = [piece(Color, rook, A/B) | Board3 ].

% update_move(Board, Piece, NewPosition, ResultBoard)
% update the Board to ResultBoard, based on a move of Piece to NewPosition
% here: the more general case, move the given piece to a new location without much extra
update_move(Board, piece(Color, Piece, X/Y), A/B, ResultBoard) :- 
    select(piece(Color, Piece, X/Y), Board, Board2), % remove old position from the board
    ResultBoard = [piece(Color, Piece, A/B) | Board2 ]. % add the new position

% update_board(Board, Piece, NewPosition, ResultBoard)
% update the Board to ResultBoard, based on a move of Piece to NewPosition
% then make sure king is not checked, en passant pieces are removed if needed, and/or castling blockers are added if needed
update_board(Board, piece(Color, Piece, X/Y), A/B, ResultBoard) :-
    update_move(Board, piece(Color, Piece, X/Y), A/B, Board2),
    \+ king_is_check(Board2, Color),
    opponent(Color, Opponent),
    delete(Board2, en_passant(Opponent, _), Board3), % opponent en passant pieces should be removed
    add_castling_blockers(Board3, Opponent, A/B, ResultBoard). %in case tower gets taken by the opponent

% --------------------------------------------- MOVE LOGIC ------------------------------------------------------

% move(Board, Piece, NewPosition, ResultBoard)
% if Piece can go to NewPosition according to the way Piece moves, and it is a valid move, update Board to ResultBoard
% here: separate case for pawn takes, no need to check valid move as select will give False if no piece could be taken 
move(Board, piece(Color, pawn, X/Y), A/B, ResultBoard) :-
    possible_take(piece(Color, pawn, X/Y), A/B),
    opponent(Color, Opponent),
    select(piece(Opponent, _, A/B), Board, Board2), % remove taken piece from the board
    update_board(Board2, piece(Color, pawn, X/Y), A/B, ResultBoard).

% move(Board, Piece, NewPosition, ResultBoard)
% if Piece can go to NewPosition according to the way Piece moves, and it is a valid move, update Board to ResultBoard
% here: pawn taking en passant 
move(Board, piece(Color, pawn, X/Y), A/B, ResultBoard) :-
    possible_take(piece(Color, pawn, X/Y), A/B),
    opponent(Color, Opponent),
    select(en_passant(Opponent, A/B), Board, Board2), % take the en_passant piece 
    possible_move(piece(Opponent, pawn, A/B), PX/PY), % get the coordinates of the piece in front of the en_passant square
    select(piece(Opponent, _, PX/PY), Board2, Board3), % remove taken piece from the board
    update_board(Board3, piece(Color, pawn, X/Y), A/B, ResultBoard).

% move(Board, Piece, NewPosition, ResultBoard)
% if Piece can go to NewPosition according to the way Piece moves, and it is a valid move, update Board to ResultBoard
% here: king castling
move(Board, piece(Color, king, X/Y), A/B, ResultBoard) :-
    possible_castle(piece(Color, king, X/Y), A/B),
    rook_side(A, RookX), 
    Rook = piece(Color, rook, RookX/Y),
    valid_castle(Board, piece(Color, king, X/Y), Rook, A/B), 
    update_move(Board, piece(Color, king, X/Y), A/B, Board2),
    rook_castle(Color, A/B, NewX/NewY),
    update_board(Board2, Rook, NewX/NewY, ResultBoard).

% move(Board, Piece, NewPosition, ResultBoard)
% if Piece can go to NewPosition according to the way Piece moves, and it is a valid move, update Board to ResultBoard
% here: general case for all pieces besides pawns
move(Board, piece(Color, Piece, X/Y), A/B, ResultBoard) :-
    possible_move(piece(Color, Piece, X/Y), A/B),
    valid_move(Board, piece(Color, Piece, X/Y), A/B), 
    opponent(Color, Opponent),
    delete(Board, piece(Opponent, _, A/B), Board2), % delete opponent piece if necessary 
    update_board(Board2, piece(Color, Piece, X/Y), A/B, ResultBoard).

% ----------------------------------------------- EXTRA HELPER LOGIC -----------------------------------------------------------

% en_passant_row(Color, Number)
% True if Number is the row number of where a pawn could have been moved skipping a square, given a Color
en_passant_row(black, 5).
en_passant_row(white, 4).

% en_passant_square(Piece, Position)
% Given a Piece, sets Position to the coordinates of where an en_passant linked to that Piece could be placed
en_passant_square(piece(black, pawn, X/Y), X/B) :- B is Y + 1.
en_passant_square(piece(white, pawn, X/Y), X/B) :- B is Y - 1.

% trade_pawn(PieceType)
% True if pawn can be trader for PieceType when pawn reaches opposite side
trade_pawn(queen).
trade_pawn(knight).
trade_pawn(bishop).
trade_pawn(rook).

% castle_side(Side, ColumnNumber)
% True if castling to Side means moving the king to ColumnNumber
castle_side(queen, 3).
castle_side(king, 7).

% color_row(Color, RowNumber)
% True if Color's closest row is RowNumber (row where the king starts)
color_row(black, 8).
color_row(white, 1).

% possible_castle(Piece, Position)
% True if Piece can move to Position, which would be a castling move
possible_castle(piece(Color, king, 5/Y), X/Y) :- color_row(Color, Y), castle_side(_, X). 

% rook_castle(Color, KingNewPosition, RookNewPosition)
% sets RookNewPosition to the position the rook should take when king castles to KingNewPosition
% Color decides which row the rook is on 
rook_castle(Color, 7/Y, 6/Y) :- color_row(Color, Y).
rook_castle(Color, 3/Y, 4/Y) :- color_row(Color, Y).

% rook_side(KingColumn, RookColumn)
% if king castles to KingColumn, we need the rook on RookColumn
rook_side(7,8).
rook_side(3,1).

% get_blocker(Color, Side, Blocker)
% given a Color and castling Side, generate the Blocker needed
get_blocker(Color, Side, Blocker) :-
    castle_side(Side, X),
    color_row(Color, Y),
    Blocker = castle_blocker(Color, X/Y).

% blocked_column(ColumnNumber, BlockedColumnNumber)
% given a ColumnNumber (this shows which side we are on), give which column should be blocked 
% castle_blocker can then come on BlockedColumnNumber
blocked_column(X, BlockedX) :- X < 5, castle_side(queen, BlockedX).
blocked_column(X, BlockedX) :- X > 5, castle_side(king, BlockedX).


:- module(piece_moves, [possible_move/2, possible_take/2, on_same_diagonal/2]).
:- use_module(chess_board).
:- use_module(utilities).

% possible_move(Piece, NewPosition)
% True if a Piece can do a move to NewPosition
% here Piece is of type rook, and can go horizontal and vertical
possible_move(piece(_, rook, X/Y), A/Y) :- 
    between_in(1,8,A),
    A \= X. % piece should not be able to move to the same place

possible_move(piece(_, rook, X/Y), X/B) :- 
    between_in(1,8,B),
    B \= Y.

% possible_move(Piece, NewPosition)
% True if a Piece can do a move to NewPosition
% here Piece is of type bishop, and can go along the two diagonals
possible_move(piece(_, bishop, X/Y), A/B) :-
    between_in(1,8,A),
    between_in(1,8,B),
    on_same_diagonal(X/Y, A/B),
    X \= A, Y\= B.

% possible_move(Piece, NewPosition)
% True if a Piece can do a move to NewPosition
% here Piece is of type queen, and can do the movements of a bishop or of a rook 
possible_move(piece(_, queen, X/Y), A/B) :- possible_move(piece(_, bishop, X/Y), A/B). 
possible_move(piece(_, queen, X/Y), A/B) :- possible_move(piece(_, rook, X/Y), A/B). 

% possible_move(Piece, NewPosition)
% True if a Piece can do a move to NewPosition
% here Piece is of type king, a king can move one step in each direction 
possible_move(piece(_, king, X/Y), X/B) :- B is Y - 1. 
possible_move(piece(_, king, X/Y), X/B) :- B is Y + 1. 

possible_move(piece(_, king, X/Y), A/Y) :- A is X + 1. 
possible_move(piece(_, king, X/Y), A/Y) :- A is X - 1. 

possible_move(piece(_, king, X/Y), A/B) :- A is X - 1, B is Y - 1. 
possible_move(piece(_, king, X/Y), A/B) :- A is X + 1, B is Y - 1. 
possible_move(piece(_, king, X/Y), A/B) :- A is X - 1, B is Y + 1. 
possible_move(piece(_, king, X/Y), A/B) :- A is X + 1, B is Y + 1. 

% possible_move(Piece, NewPosition)
% True if a Piece can do a move to NewPosition
% here Piece is of type knight, and can move in the shape of an L 
possible_move(piece(_, knight, X/Y), A/B)  :- A is X + 1, B is Y + 2. 
possible_move(piece(_, knight, X/Y), A/B)  :- A is X + 1, B is Y - 2. 
possible_move(piece(_, knight, X/Y), A/B)  :- A is X - 1, B is Y - 2. 
possible_move(piece(_, knight, X/Y), A/B)  :- A is X - 1, B is Y + 2. 
possible_move(piece(_, knight, X/Y), A/B)  :- A is X + 2, B is Y + 1. 
possible_move(piece(_, knight, X/Y), A/B)  :- A is X + 2, B is Y - 1. 
possible_move(piece(_, knight, X/Y), A/B)  :- A is X - 2, B is Y + 1. 
possible_move(piece(_, knight, X/Y), A/B)  :- A is X - 2, B is Y - 1. 

% possible_move(Piece, NewPosition)
% True if a Piece can do a move to NewPosition
% here Piece is of type pawn, and can move to the front one step 
possible_move(piece(white, pawn, X/Y), X/B) :- B is Y+1.
possible_move(piece(black, pawn, X/Y), X/B) :- B is Y-1.

% possible_move(Piece, NewPosition)
% True if a Piece can do a move to NewPosition
% here Piece is of type pawn, and can move to the front two steps if it's in their initial position 
possible_move(piece(white, pawn, X/Y), X/B) :- init_piece(piece(white, pawn, X/Y)), B is Y+2. %if pawn is in its initial place, it can move 2 squares
possible_move(piece(black, pawn, X/Y), X/B) :- init_piece(piece(black, pawn, X/Y)), B is Y-2.

% possible_take(Piece, NewPosition)
% True if a Piece can move to take a piece at NewPosition
% here Piece is of type pawn, and can only take sideways 
possible_take(piece(white, pawn, X/Y), A/B) :- A is X+1, B is Y+1.
possible_take(piece(white, pawn, X/Y), A/B) :- A is X-1, B is Y+1.

possible_take(piece(black, pawn, X/Y), A/B) :- A is X+1, B is Y-1.
possible_take(piece(black, pawn, X/Y), A/B) :- A is X-1, B is Y-1.

% possible_take(Piece, NewPosition)
% True if a Piece can move to take a piece at NewPosition
% here Piece is of any type besides pawn, and can take any way it can move
possible_take(Piece, A/B) :- Piece \= piece(_, pawn, _), possible_move(Piece,A/B).

% on_same_diagonal(Position, OtherPosition)
% True if two squares, Position and OtherPosition, are on the same diagonal
on_same_diagonal(X/Y, A/B) :-
    diagonal(X/Y, Diagonal, Type),
    diagonal(A/B, OtherDiagonal, Type),
    Diagonal = OtherDiagonal.

% diagonal(Position, Diagonal, TypeNumber)
% sets Diagonal to the value of the diagonal calculation of a Position
% TypeNumber keeps track of which diagonal has been calculated 
diagonal(X/Y, Diagonal,1) :- Diagonal is X + Y.
diagonal(X/Y, Diagonal,2) :- Diagonal is X - Y.
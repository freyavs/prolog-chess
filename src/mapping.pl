:- module(mapping, [ symbol_to_piece/3, letter_number/2, symbol/3, player/1]).

% symbol_to_piece( ChessCharacter, Column/Row, Piece)
% map between a chess input character ChessCharacter and a Piece
% Column/Row is the location of the piece
symbol_to_piece(♙, X/Y, piece(white, pawn, X/Y)). 
symbol_to_piece(♟, X/Y, piece(black, pawn, X/Y)). 

symbol_to_piece(♗, X/Y, piece(white, bishop, X/Y)). 
symbol_to_piece(♝, X/Y, piece(black, bishop, X/Y)). 

symbol_to_piece(♘, X/Y, piece(white, knight, X/Y)). 
symbol_to_piece(♞, X/Y, piece(black, knight, X/Y)). 

symbol_to_piece(♖, X/Y, piece(white, rook, X/Y)). 
symbol_to_piece(♜, X/Y, piece(black, rook, X/Y)). 

symbol_to_piece(♕, X/Y, piece(white, queen, X/Y)). 
symbol_to_piece(♛, X/Y, piece(black, queen, X/Y)). 

symbol_to_piece(♔, X/Y, piece(white, king, X/Y)). 
symbol_to_piece(♚, X/Y, piece(black, king, X/Y)). 

% letter_number(Letter, Number)
% map between a Letter and a Number of the chess board
letter_number('a', 1).
letter_number('b', 2).
letter_number('c', 3).
letter_number('d', 4).
letter_number('e', 5).
letter_number('f', 6).
letter_number('g', 7).
letter_number('h', 8).

% symbol(Color, PieceType, Character)
% Character represents the character of a Color and PieceType
symbol(white, pawn, "\u2659"). 
symbol(black, pawn, "\u265F"). 

symbol(white, bishop, "\u2657"). 
symbol(black, bishop, "\u265D"). 

symbol(white, knight, "\u2658"). 
symbol(black, knight, "\u265E"). 

symbol(white, rook, "\u2656"). 
symbol(black, rook, "\u265C"). 

symbol(white, queen, "\u2655"). 
symbol(black, queen, "\u265B"). 

symbol(white, king, "\u2654"). 
symbol(black, king, "\u265A"). 

% player(Character)
% Character represents the character representing a player's turn
player("\u261A").

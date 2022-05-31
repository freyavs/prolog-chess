:- module(output, [ output_game/1 ]).
:- use_module(chess_board).
:- use_module(mapping).

% output_game(Game)
% write Game to the terminal
output_game(Game) :-
    write("8 "),
    output_helper(Game, 1, 8).

% output_game(Game)
% When game is "draw", output should be different from normal output
output_game(draw) :- write("DRAW").

% output_helper(Game, ColumnNumber, RowNumber)
% when RowNumber is 0, writing of the game is done, so write a final newline 
output_helper(_, _, 0) :- !, nl. 

% output_helper(Game, ColumnNumber, RowNumber)
% when RowNumber is equal to the row where extra info should be printed and ColumnNumber is 9
% extract info from Game and write it
output_helper(game(Board, Player), 9, Y) :-
    info_row(Y, Color),
    !,
    write(" ["),
    castle_symbol(Board, Color, queen, Queen),
    castle_symbol(Board, Color, king, King),
    write(Queen),
    write(King),
    output_enpassant(Board, Color),
    write("]"),
    output_player(Player, Color),
    nl,
    X = 1,
    NewY is Y - 1,
    draw_new_line(NewY),
    output_helper(game(Board, Player), X, NewY). 

% output_helper(Game, ColumnNumber, RowNumber)
% write the piece on the board of Game that's found on ColumnNumber and RowNumber
% then continue writing the row
output_helper(game(Board, Player), X, Y) :-
    X < 9,
    draw(Board, X, Y),
    NextX is X + 1,
    output_helper(game(Board, Player), NextX, Y).

% output_helper(Game, ColumnNumber, RowNumber)
% ColumnNumber is higher than the number of columns on a chess board
% reset ColumnNumber and start writing next row
output_helper(Game, 9, Y) :-
    nl,
    X = 1,
    NewY is Y - 1,
    draw_new_line(NewY),
    output_helper(Game, X, NewY). 

% draw(Board, ColumnNumber, RowNumber)
% output the piece that is found on given RowNumber and ColumnNumber
draw(Board, X,Y) :-
    member(piece(Color, Piece, X/Y), Board),
    !,
    symbol(Color, Piece, Symbol),
    write(Symbol).

% draw(Board, ColumnNumber, RowNumber)
% if no piece is found on ColumnNumber and RowNumber, output a space
draw(_, _, _) :- write(" ").

% draw_new_line(RowNumber)
% if RowNumber is outside the chess board, output the last line
draw_new_line(0) :- !, write("  abcdefgh"). 

% draw_new_line(RowNumber)
% output the RowNumber and a space for the start of a new line
draw_new_line(Y) :- 
    Y =< 8, 
    write(Y),
    write(" ").

% output_enpassant(Board, Color) :-
% for a given Color, output the en passant pieces on the Board
output_enpassant([ Piece | RestBoard ], Color) :-
    Piece = en_passant(Color, X/Y),!,
    letter_number(L, X),
    write(L),
    write(Y),
    output_enpassant(RestBoard, Color).
    
% output_enpassant(Board, Color) :-
% if head of Board list is not an en passant piece, skip it and check the rest
output_enpassant([ _ | RestBoard ], Color) :-
    output_enpassant(RestBoard, Color).

% output_enpassant(Board, Color) :-
% True if Board is empty
output_enpassant([], _).

% output_player(PlayerColor, InfoColor)
% if color of next player, PlayerColor, is the same as the color of the info that needs to be written, InfoColor
% output the player symbol
output_player(Player, Color) :- 
    Player = Color, !,
    player(Symbol),
    write(Symbol).

% output_player(PlayerColor, InfoColor)
% do nothing if PlayerColor and InfoColor are not equal
output_player(_, _).

% info_row(Number, Color)
% True if Number is the row where the info of Color is found
info_row(1, white).
info_row(8, black).

% castle_symbol(Board, Color, Side, Symbol)
% Side represents which side we are castling on, queen or king side
% sets Symbol to a space if castling is blocked on the Board, given Color of the player
castle_symbol(Board, Color, Side, " ") :-
    castle_side(Side, X),
    color_row(Color, Y),
    member(castle_blocker(Color, X/Y), Board), !.

% castle_symbol(Board, Color, Side, Symbol)
% Side represents which side we are castling on, queen or king side
% sets Symbol to a correct symbol if castling is not blocked on the Board, given Color of the player
castle_symbol(_, Color, Side, Symbol) :-
    symbol(Color, Side, Symbol).
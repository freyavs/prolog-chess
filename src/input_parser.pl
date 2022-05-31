:- module(input_parser, [init_game/2]).
:- use_module(library(pio)).
:- use_module(chess_board).
:- use_module(mapping).

% ------------------- basic parser functions --------------------------

% parse a number
digit(D) --> [D], { char_type(D, digit) }.

% parse some character that is not a newline or bracket
one_character(D) --> [D], { \+ char_type(D, newline), \+ is_bracket(D) }.

% parse a bracket
bracket(B) --> [B], { is_bracket(B) }.

% True if a character is a bracket
is_bracket(B) :- char_code(']', B).
is_bracket(B) :- char_code('[', B).

% parse whitespace
ws --> [].
ws --> space, ws.

% parse input of type space
space --> [S], { char_type(S, space) }.

% parse an optional non digit, this optional is only needed for the player symbol
optional(black) --> ws.
optional(white) --> one_character(_).

% parse letter(s)
alpha(A) --> [A], { char_type(A, alpha) }.

alphas([L|Ls]) --> alpha(L), alphas(Ls).
alphas([])     --> [].


% ------------------- chess parser functions --------------------------

% parse the last row, "abcdefgh" 
last(_) -->
    ws,
    alpha(_),
    alphas(_),
    ws.

% parse the castling and en passant information in [ .. ] given a certain color 
info_part(Color, Result) -->
    bracket(_),
    { Info = []},
    castle_piece(Color, queen, Info,Info2),
    castle_piece(Color, king, Info2, Info3),
    enpassant_pieces(Color, E),
    bracket(_),
    optional(_),
    { append(E, Info3, Result)}.

% parse all the pieces on a row of the input board
row(Result) -->
    digit(Number),
    space,
    pieces(T, 1/RowNumber),
    space,
    info(RowNumber, InfoList),
    {  char_code(N, Number),
       atom_number(N, RowNumber),
       append(InfoList, T, Result)}.


% pass all en passant pawns
enpassant_pieces(Color, [P|Ls]) --> enpassant_piece(Color, P), enpassant_pieces(Color, Ls).
enpassant_pieces(_, []) --> [].

% parse an en passant pawn
enpassant_piece(Color, P) --> 
    alpha(Y), 
    digit(X), 
        { 
        char_code(X1,X),
        char_code(Y1,Y),
        atom_number(X1, X2),
        letter_number(Y1, Y2),
        P = en_passant(Color, X2/Y2) }.

% parse the info between [ ... ] for a certain color
info(1, Result) --> info_part(white, Result). 
info(8, Result) --> info_part(black, Result). 
info(_, []) --> ws. 

% parse castling
castle_piece(Color, Side, Info, [ B | Info ]) --> one_character(C), {char_type(C, space), get_blocker(Color,Side, B)}. 
castle_piece(_, _, Info, Info) --> one_character(C), {\+ char_type(C, space)}. 

% parse the pieces with the correct square coordinates (row and column)
pieces([P|Ls], X/Y) --> one_character(L), {NewX is X + 1}, pieces(Ls, NewX/Y), { char_code(C, L), symbol_to_piece(C, X/Y, P)}.
pieces(Ls, X/Y) --> one_character(L), {NewX is X + 1, char_type(L, space)}, pieces(Ls, NewX/Y).
pieces([],_)     --> [].

% parse the board
read_board([])     --> [].
read_board([T|Ts]) --> row(T), ws, read_board(Ts).

% parse the game
read_game(game(R, Player)) --> read_board(T), optional(Player), last(_), {append(T, R)}.

% init_game(Game, InputString)
% set Game to the game read from InputString
init_game(Game, Str) :- 
    string_codes(Str,Codes),
    phrase(read_game(Game), Codes), !.

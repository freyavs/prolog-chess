:- begin_tests(minimax).
:- use_module('../src/minimax').
:- use_module('../src/game_handler').
:- use_module('../src/output').

% test backrank checkmate for white player
test(backrank_checkmate_in_white) :-
    Board = [ piece(black, king, 1/8), piece(black, pawn,1/7), piece(black, pawn, 2/7), piece(white, king, 1/1), piece(white, rook, 4/1)], 
    get_best_move(game(Board, white), game(Result,_)),
    member(piece(white, rook, 4/8), Result).

% test backrank checkmate for black player
test(backrank_checkmate_in_black) :-
    Board = [ piece(white, king, 1/1), piece(white, pawn,1/2), piece(white, pawn, 2/2), piece(black, king, 8/8), piece(black, rook, 4/8)], 
    get_best_move(game(Board, black), game(Result,_)),
    member(piece(black, rook, 4/1), Result).

% example used in the report, output games so you see what happens
test(example_report) :-
    Board = [ piece(white, king, 7/1), piece(white, pawn, 1/6), piece(black, king, 2/8), piece(white, queen, 1/1) ],
    get_best_move(game(Board, white), Game),
    output_game(Game),
    get_best_move(Game, Next),
    output_game(Next),
    get_best_move(Next, Next2),
    output_game(Next2),
    get_best_move(Next2, Next3),
    output_game(Next3),
    get_best_move(Next3, Next4),
    output_game(Next4).

:- end_tests(minimax).
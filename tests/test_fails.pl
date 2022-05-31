:- begin_tests(test_fails).
:- use_module('../src/game_handler').
:- use_module('../src/chess_board').

% test for a problem when running the random player, where king put himself in check
test(king_should_not_check_itself) :-
    Board = [castle_blocker(white,7/1),castle_blocker(white,3/1),castle_blocker(black,7/8),castle_blocker(black,3/8),piece(white,knight,2/1),piece(white,bishop,3/1),piece(white,queen,4/1),piece(white,bishop,6/1),piece(white,knight,7/1),piece(white,king,5/3),piece(black,bishop,6/4),piece(black,pawn,3/5),piece(black,pawn,6/5),piece(white,pawn,7/5),piece(white,rook,8/5),piece(white,pawn,1/6),piece(black,pawn,2/6),piece(black,pawn,5/6),piece(black,pawn,7/6),piece(white,pawn,8/6),piece(black,pawn,1/7),piece(black,pawn,2/7),piece(black,bishop,4/7),piece(black,knight,5/7),piece(black,pawn,8/7),piece(black,rook,1/8),piece(black,king,4/8),piece(black,knight,5/8),piece(black,queen,6/8),piece(black,rook,8/8)],
    findall(N/M, move(Board, piece(white, king, 5/3), N/M, _), ValidMoves),
    \+ member(4/4, ValidMoves),
    \+ member(5/4, ValidMoves).


:- end_tests(test_fails).
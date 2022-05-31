:- module(scoring, [utility/3, max_to_move/1, min_to_move/1]).
:- use_module(chess_board).

% utility(Game, Utility)
% Game is an input board represented as game(Board, NextPlayer)
% NextPlayer is the next player to make a move, the other player just made a move on Board
% utility of a Game calculates the Utility of Game, which represents a score of how good or bad the board is
utility(game(Board, NextPlayer), UtilityScore, Depth) :-
    opponent(NextPlayer, JustPlayed),
    score(Board, JustPlayed, Utility, Depth),
    update_score(JustPlayed, Utility, UtilityScore).


% score(Board, Color, Score)
% calculates the Score of a Board given a Color to calculate it for
% when in end states, give special scores
score(Board, Color, 0, _) :- opponent(Color, Opponent), is_in_stalemate(Board,Opponent), !. % draw
score(Board, Color, Score, Depth) :- opponent(Color, Opponent), is_in_checkmate(Board,Opponent), X is Depth+1, Score is X*200, !. % win

% score(Board, Color, Score)
% calculates the Score of a Board given a Color to calculate it for
% when not in an endstate, calculate score based on the placement of the pieces on Board 
score(Board, Color, Score, _) :- 
    findall(piece(Color, P, X/Y), member(piece(Color, P,X/Y), Board), Pieces),
    board_score(Pieces, BoardScore), 
    center_score(Pieces, CenterScore),
    moved_score(Pieces, MovedScore),
    Score1 is BoardScore + CenterScore,
    Score is Score1 + MovedScore.

% moved_score(Board, Color, Score) 
% calculates the Score of Board given a Color, score is based on how many pieces have moved form their initial position
moved_score(Pieces, Score) :-
    convlist(piece_moved_score, Pieces, Scores),
    sum_list(Scores, Score).

% center_score(Board, Color, Score) 
% calculates the Score of Board given a Color, score is based on how many pieces are in a more centered position 
center_score(Pieces, Score) :-
    convlist(square_score, Pieces, Scores),
    sum_list(Scores, Score).

% center_score(Board, Color, Score) 
% calculates the Score of Board given a Color, score is based on piece scores used in chess 
board_score(Pieces, Score) :-
    convlist(piece_score, Pieces, Scores),
    sum_list(Scores, Score).

% piece_score(Piece, Score)
% gives the Score of a Piece, Score is a value of a certain piece, based on values used in chess +1 
piece_score(piece(_,pawn,_), 2).
piece_score(piece(_,knight,_), 4).
piece_score(piece(_,bishop,_), 4).
piece_score(piece(_,rook,_), 6).
piece_score(piece(_,queen,_), 10).
piece_score(piece(_,king,_), 0).

% piece_moved_score(Piece, Score)
% gives a Score based on if Piece is still in initial position.
% Moving the king or towers should be neutral / worse than other pieces as we want to save castling possibilities.
piece_moved_score(piece(_, king, _), 0):- !.
piece_moved_score(piece(_, rook, _), 0):- !. 

% piece_moved_score(Piece, Score)
% gives a Score based on if Piece is still in initial position.
% Give a negative Score if piece is in its initial position, else Score should be positive.
piece_moved_score(Piece, -1) :- init_piece(Piece), !.
piece_moved_score(_, 2).

% square_score(Column/Row, Score)
% calculates a Score of a position, represented as Column/Row on the chess board
% a square closer to the center of the board gives a higher score, except for the king who should avoid the center
square_score(piece(_, king, X/Y), Score) :- !, row_or_column_score(X, RowScore), row_or_column_score(Y, ColScore), S is RowScore + ColScore, Score is 0 - S.
square_score(piece(_, _, X/Y), Score) :- row_or_column_score(X, RowScore), row_or_column_score(Y, ColScore), Score is RowScore + ColScore.

% row_or_column_score(Number, Score)
% Number represents either a row or column of the chess board
% given a Number, Score should be higher when row or column is more towards the center.
row_or_column_score(1, 0). 
row_or_column_score(8, 0). 

row_or_column_score(2, 1). 
row_or_column_score(7, 1). 

row_or_column_score(3, 2). 
row_or_column_score(6, 2). 

row_or_column_score(4, 3). 
row_or_column_score(5, 3). 

% max_to_move(Game)
% True if white is next to move
max_to_move(game(_,white)).

% min_to_move(Game)
% True if black is next to move
min_to_move(game(_,black)).

% update_score(Color, Utility, UpdatedUtility)
% Negate the score depending of Color is min or max player
update_score(Color, Utility, Utility) :- max_to_move(game(_,Color)), !.
update_score(_, Utility, Negated) :- Negated is 0 - Utility.

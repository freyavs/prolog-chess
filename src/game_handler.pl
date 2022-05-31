:- module(game_handler, [handle_game/2, get_result/3, get_best_move/2]).
:- use_module(chess_board).
:- use_module(output).
:- use_module(minimax).

% handle_game(Game, ArgumentList)
% prints the best game given ArgumentList is empty
handle_game(Game, []) :- !, get_best_move(Game, Best), output_game(Best).

% handle_game(Game, ArgumentList)
% prints all possible games given ArgumentList is  not empty (in this case it contains TEST)
handle_game(Game, _) :- generate_moves(Game).

% get_best_move(InputGame, BestNextGame)
% given InputGame as game(Board, Player), set BestNextGame as game with best possible move played for Player
get_best_move(Game, BestNext) :-
    minimax(Game, BestNext, _, 3, -1000, 1000), !.

% generate_moves(Game)
% print all possible moves for a given Game
generate_moves(game(Board, Player)) :-
    findall(R, get_result(Board, Player, R), Results),
    length(Results, Length),
    Length > 0, !,
    opponent(Player, Opponent),
    print_results(Results, Opponent).

% generate_moves(Game)
% no next moves found for Game, game is drawn 
generate_moves(_) :- output_game(draw).

% print_results(BoardList, NextPlayer)
% print all games, given all boards in a BoardList and player who is next to move, NextPlayer
print_results([], _).

print_results([R], NextPlayer) :- 
    !,
    output_game(game(R, NextPlayer)).

print_results([ R | Results ], NextPlayer) :-
    output_game(game(R, NextPlayer)),
    write("~"), nl,
    print_results(Results, NextPlayer).

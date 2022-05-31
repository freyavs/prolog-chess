:- module(minimax, [minimax/6]).
:- use_module(chess_board).
:- use_module(game_handler).
:- use_module(scoring).

% minimax(InputGame, BestNextGame, Value, Depth, Alpha, Beta)
% in this case, we have reached maximum depth (Depth = 0) in the minimax tree, so set its utility as Value
% BestNextGame is "draw" for when we have reached this state from the start, else it will be discarted anyway
minimax(Game, draw, Value, 0, _, _) :-
    !,
    utility(Game, Value, 0).

% minimax(InputGame, BestNextGame, Value, Depth, Alpha, Beta)
% computes the BestNextGame given an InputGame, where Player is next to move
% Value is the value or score of the BestNextGame
% Depth is a counter, counting down until 0, representing the depth we have reached in the minimax tree
% Alpha and Beta are the min and max values for the alpha-beta pruning
minimax(game(Board, Player), BestNext, Value, Depth, Alpha, Beta) :-
    opponent(Player, Opponent),
    NextDepth is Depth - 1,
    findall(game(R, Opponent), get_result(Board, Player, R), Results),
    \+ length(Results, 0),!,
    best(Results, BestNext, Value, NextDepth, Alpha, Beta), !.

% minimax(InputGame, BestNextGame, Value, Depth, Alpha, Beta)
% in this case, Game has no successors, so set its utility as Value
% BestNextGame is "draw" for when we have reached this state from the start, else it will be discarted anyway
minimax(Game, draw, Value, Depth, _, _) :-
    utility(Game, Value, Depth).

% best(GameList, BestGame, BestValue, Depth, Alpha, Beta)
% given a list of games, GameList, compute the BestGame with value BestValue
% Depth is de depth counter, Alpha and Beta are the alpha-beta pruning values
best([ Game | Games ], BestGame, BestValue, Depth, Alpha, Beta) :-
    minimax(Game, _, Value, Depth, Alpha, Beta),
    best_pruned(Games, Game, Value, BestGame, BestValue, Depth, Alpha, Beta).

% best_pruned(GameList, Game, Value, BestGame, BestValue, Depth, Alpha, Beta)
% no more candidates
best_pruned([] ,Game, Value, Game, Value,_, _, _) :- !.               

% best_pruned(GameList, Game, Value, BestGame, BestValue, Depth, Alpha, Beta)
% either max player reached the upper bound, or min player reached the lower bound
best_pruned(_,  Game, Value, Game, Value, _, _, Beta) :-
    min_to_move(Game), Value > Beta, !.

best_pruned(_,  Game, Value, Game, Value, _, Alpha, _) :-
    max_to_move(Game), Value < Alpha, !.

% best_pruned(GameList, Game, Value, BestGame, BestValue, Depth, Alpha, Beta)
% computes the BestGame and its BestValue given a GameList and a Game and its Value
% Depth is the depth counter, Alpha and Beta are the alpha-beta pruning values 
% best_pruned takes Alpha and Beta into consideration, and updates them when needed
best_pruned( GameList,  Game, Value, BestGame, BestValue, Depth, Alpha, Beta) :-
    update_bounds( Alpha, Beta, Game, Value, NewAlpha, NewBeta), 
    best(GameList, OtherGame, OtherValue, Depth, NewAlpha, NewBeta),
    better_of(Game, Value, OtherGame, OtherValue, BestGame, BestValue).

% update_bounds(Alpha, Beta, Game, Value, NewAlpha, NewBeta)
% update (if necessary) current Alpha and Beta given a Game and its Value, to new values NewAlpha and NewBeta
% min player lowered the upper bound
update_bounds(Alpha, Beta, Game, Value, Value, Beta) :-
    min_to_move(Game), Value > Alpha, !. 

% max player incremented the lower bound
update_bounds(Alpha, Beta, Game, Value, Alpha, Value):-
    max_to_move(Game), Value < Beta, !.

update_bounds(Alpha,Beta, _,_,Alpha, Beta). 

% better_of(Game, Value, OtherGame, OtherValue, BestGame, BestValue)
% set BestGame and BestValue as best from two games, namely Game and Value vs. OtherGame and OtherValue
better_of(Game1, Val1, _, Val2, Game1, Val1) :-
    min_to_move(Game1),
    Val1 > Val2, !
    ;
    max_to_move(Game1),
    Val1 < Val2, !.

better_of(_,_, Game2, Val2, Game2, Val2).

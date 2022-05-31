:- initialization(main).
:- use_module(input_parser).
:- use_module(game_handler).

% main function of the chess engine
% given an input board, it either outputs all next boards or the best board depending which argument is given
main:-
    read_string(user_input, _, Str),
    current_prolog_flag(argv, Args),
    init_game(G, Str),
    handle_game(G, Args),
    halt(0).


% Intermediate game state 1
intermediate_state_1(GameState) :-
    Board = [
        [empty, empty, empty, empty, empty, empty, empty],
        [empty, empty, empty, empty, empty, empty, empty],
        [empty, empty, white, empty, empty, empty, empty],
        [empty, black, empty, white, empty, empty, empty],
        [empty, empty, empty, empty, black, empty, empty],
        [empty, empty, empty, empty, empty, empty, empty],
        [empty, empty, empty, empty, empty, empty, empty]
    ],
    GameState = state(Board, white).

% Start game from intermediate state 1
start_intermediate_state_1 :-
    intermediate_state_1(GameState),
    game_loop((player2, human, human), GameState, Winner), display_winner(MatchState, Winner).

% Intermediate game state 2
intermediate_state_2(GameState) :-
    Board = [
        [empty, empty, empty, empty, empty, empty, empty],
        [empty, empty, empty, empty, empty, empty, empty],
        [empty, white, white, empty, empty, empty, empty],
        [empty, black, empty, white, empty, empty, empty],
        [empty, empty, black, empty, black, empty, empty],
        [empty, empty, empty, empty, empty, empty, empty],
        [empty, empty, empty, empty, empty, empty, empty]
    ],
    GameState = state(Board, black).

% Start game from intermediate state 2
start_intermediate_state_2 :-
    intermediate_state_2(GameState),
    game_loop((player2, human, human), GameState, Winner), display_winner(MatchState, Winner).

% Near-final game state 1
near_final_state(GameState) :-
    Board = [
        [stack(white), stack(white), white, empty, empty, empty, empty],
        [empty, empty, empty, empty, empty, empty, empty],
        [empty, empty, white, empty, empty, empty, empty],
        [empty, empty, empty, empty, empty, empty, black],
        [empty, empty, empty, empty, empty, empty, black],
        [empty, empty, empty, empty, stack(black), stack(black), empty],
        [empty, empty, empty, empty, empty, empty, empty]
    ],
    GameState = state(Board, white).

% Start game from near-final state 1
start_near_final_state :-
    near_final_state(GameState),
    game_loop((player2, human, human), GameState, Winner), display_winner(MatchState, Winner).

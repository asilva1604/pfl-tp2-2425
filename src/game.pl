:- use_module(library(lists)).
% Initializes the game state for LOT.
% GameConfig: Contains additional configuration options if needed (e.g., future extensions).
% GameState: The initial state of the game.

initial_state(GameConfig, GameState) :-
    % Create the 7x7 empty board.
    create_board(7, Board),
    % Define the initial game state.
    GameState = state(Board, white).

% Helper predicate to create an empty board of a given size.
create_board(Size, Board) :-
    length(Board, Size),                % Create a list of rows.
    maplist(create_row(Size), Board).   % Each row is a list of 'empty' cells.

% Helper predicate to create a row with 'empty' cells.
create_row(Size, Row) :-
    length(Row, Size),                  % Create a list of cells.
    maplist(=(empty), Row).             % Initialize all cells to 'empty'.

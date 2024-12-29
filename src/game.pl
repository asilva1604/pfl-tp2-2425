:- use_module(library(lists)).
:- use_module(library(between)).
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

% Displays the current game state: the board and the current player.
display_game(state(Board, Player)) :-
    write('Current player: '), writeln(Player),
    write('  1   2   3   4   5   6   7  '), nl, % Column headers
    display_rows(Board, 1).

% Helper predicate to display rows with row numbers.
display_rows([], _).
display_rows([Row | Rest], RowNum) :-
    write(RowNum), write(' '), % Row header
    display_row(Row), nl,
    NextRowNum is RowNum + 1,
    display_rows(Rest, NextRowNum).

% Helper predicate to display a single row.
display_row([]).
display_row([Cell | Rest]) :-
    symbol_for_cell(Cell, Symbol),
    write(Symbol), write(' '),
    display_row(Rest).

% Maps cell values to display symbols.
symbol_for_cell(empty, '.  ').
symbol_for_cell(white, 'W  ').
symbol_for_cell(black, 'B  ').
symbol_for_cell(white_stack, 'WW ').
symbol_for_cell(black_stack, 'BB ').

writeln(X) :-
    write(X),
    nl.

% Applies a move to the game state, resulting in a new game state.
move(state(Board, Player), (Row, Col), state(NewBoard, NextPlayer)) :-
    valid_position(Board, Row, Col),          % Ensure the position is valid.
    nth1(Row, Board, CurrentRow),            % Get the target row.
    nth1(Col, CurrentRow, empty),            % Ensure the target cell is empty.
    replace(Board, Row, Col, Player, NewBoard), % Update the board with the player's piece.
    switch_player(Player, NextPlayer).       % Switch the player.

% Ensures the position (Row, Col) is valid.
valid_position(Board, Row, Col) :-
    length(Board, Size),                     % Get the board size.
    between(1, Size, Row),                   % Check Row is within bounds.
    between(1, Size, Col).                   % Check Col is within bounds.

% Replaces the cell at (Row, Col) with the player's piece.
replace(Board, Row, Col, Player, NewBoard) :-
    nth1(Row, Board, CurrentRow),
    replace_in_list(CurrentRow, Col, Player, NewRow), % Replace in the target row.
    replace_in_list(Board, Row, NewRow, NewBoard).    % Replace the updated row.

% Helper to replace an element in a list.
replace_in_list([_|Rest], 1, Elem, [Elem|Rest]).
replace_in_list([X|Rest], N, Elem, [X|NewRest]) :-
    N > 1,
    N1 is N - 1,
    replace_in_list(Rest, N1, Elem, NewRest).

% Switches the player.
switch_player(white, black).
switch_player(black, white).


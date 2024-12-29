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
symbol_for_cell(stack(white), 'WW ').
symbol_for_cell(stack(black), 'BB ').

writeln(X) :-
    write(X),
    nl.

% Applies a move to the game state, resulting in a new game state.
move(state(Board, Player), (Row, Col), state(NewBoard, NextPlayer)) :-
    valid_position(Board, Row, Col),          % Ensure the position is valid.
    nth1(Row, Board, CurrentRow),            % Get the target row.
    nth1(Col, CurrentRow, empty),            % Ensure the target cell is empty.
    replace(Board, Row, Col, Player, TempBoard), % Update the board with the player's piece.
    check_lines(TempBoard, Player, Row, Col, TempBoard2), % Check for lines of three and handle them.
    switch_player(Player, NextPlayer),       % Switch the player.
    NewBoard = TempBoard2.                   % Set the new board state.

% Checks for lines of three or more consecutive pieces and handles them.
check_lines(Board, Player, Row, Col, NewBoard) :-
    (   line_of_three(Board, Player, Row, Col, Line) ->
        handle_line(Board, Line, Player, NewBoard)
    ;   NewBoard = Board).

% Checks if placing a piece creates a line of three or more consecutive pieces.
line_of_three(Board, Player, Row, Col, Line) :-
    (   horizontal_line(Board, Player, Row, Col, Line);
        vertical_line(Board, Player, Row, Col, Line);
        diagonal_line(Board, Player, Row, Col, Line)
    ).

adjacent_horizontal(Board, Player, Row, Col, AdjCol) :-
    find_adjacent_horizontal(Board, Player, Row, Col, -1, LeftCells),
    find_adjacent_horizontal(Board, Player, Row, Col, 1, RightCells),
    append(LeftCells, RightCells, AdjacentCells),
    member(AdjCol, AdjacentCells).

% Helper predicate to find adjacent cells in a specific direction.
find_adjacent_horizontal(Board, Player, Row, Col, Direction, AdjacentCells) :-
    NextCol is Col + Direction,
    (   NextCol > 0,
        nth1(Row, Board, RowList),
        nth1(NextCol, RowList, Player)
    ->  find_adjacent_horizontal(Board, Player, Row, NextCol, Direction, RestCells),
        AdjacentCells = [NextCol | RestCells]
    ;   AdjacentCells = []
    ).

% Checks for a horizontal line of three or more consecutive pieces.
horizontal_line(Board, Player, Row, Col, Line) :-
    findall((Row, C), (adjacent_horizontal(Board, Player, Row, Col, C)), AdjacentCells),
    length(AdjacentCells, Length),
    Length >= 2,
    Line = [(Row, Col) | AdjacentCells].

% Checks for a vertical line of three or more consecutive pieces.
vertical_line(Board, Player, Row, Col, Line) :-
    findall((R, Col), (adjacent_vertical(Board, Player, Row, Col, R)), AdjacentCells),
    length(AdjacentCells, Length),
    Length >= 2,
    Line = [(Row, Col) | AdjacentCells].

% Checks for a diagonal line of three or more consecutive pieces.
diagonal_line(Board, Player, Row, Col, Line) :-
    findall((R, C), (adjacent_diagonal(Board, Player, Row, Col, R, C)), AdjacentCells),
    length(AdjacentCells, Length),
    Length >= 2,
    Line = [(Row, Col) | AdjacentCells].

% Handles a line of three by removing two pieces and creating a stack.
handle_line(Board, [(R1, C1), (R2, C2), (R3, C3) | Rest], Player, NewBoard) :-
    replace(Board, R1, C1, empty, TempBoard1),
    replace(TempBoard1, R2, C2, empty, TempBoard2),
    replace(TempBoard2, R3, C3, stack(Player), TempBoard3),
    handle_line(TempBoard3, Rest, Player, NewBoard).
handle_line(Board, [], _, Board).

% Finds adjacent cells vertically.
adjacent_vertical(Board, Player, Row, Col, AdjRow) :-
    find_adjacent_vertical(Board, Player, Row, Col, -1, UpCells),
    find_adjacent_vertical(Board, Player, Row, Col, 1, DownCells),
    append(UpCells, DownCells, AdjacentCells),
    member(AdjRow, AdjacentCells).

% Helper predicate to find adjacent cells in a specific direction.
find_adjacent_vertical(Board, Player, Row, Col, Direction, AdjacentCells) :-
    NextRow is Row + Direction,
    (   NextRow > 0,
        nth1(NextRow, Board, RowList),
        nth1(Col, RowList, Player)
    ->  find_adjacent_vertical(Board, Player, NextRow, Col, Direction, RestCells),
        AdjacentCells = [NextRow | RestCells]
    ;   AdjacentCells = []
    ).

% Finds adjacent cells diagonally.
adjacent_diagonal(Board, Player, Row, Col, AdjRow, AdjCol) :-
    (AdjRow is Row - 1, AdjCol is Col - 1;
     AdjRow is Row - 1, AdjCol is Col + 1;
     AdjRow is Row + 1, AdjCol is Col - 1;
     AdjRow is Row + 1, AdjCol is Col + 1),
    nth1(AdjRow, Board, AdjRowList),
    nth1(AdjCol, AdjRowList, Player).

% Switches the player.
switch_player(white, black).
switch_player(black, white).

% Generates a list of all valid moves for the current game state.
valid_moves(state(Board, _), Moves) :-
    findall((Row, Col), valid_position(Board, Row, Col), Moves).

% Ensures the position (Row, Col) is valid (empty and within bounds).
valid_position(Board, Row, Col) :-
    length(Board, Size),         % Get the board size.
    between(1, Size, Row),       % Ensure the row is within bounds.
    between(1, Size, Col),       % Ensure the column is within bounds.
    nth1(Row, Board, CurrentRow),
    nth1(Col, CurrentRow, empty). % Ensure the position is empty.

% Replaces the element at (Row, Col) in the board with a new element.
replace([Row|RestRows], 1, Col, Elem, [NewRow|RestRows]) :-
    replace_in_row(Row, Col, Elem, NewRow).
replace([Row|RestRows], RowIndex, Col, Elem, [Row|NewRestRows]) :-
    RowIndex > 1,
    NewRowIndex is RowIndex - 1,
    replace(RestRows, NewRowIndex, Col, Elem, NewRestRows).

% Replaces the element at Col in the row with a new element.
replace_in_row([_|RestCols], 1, Elem, [Elem|RestCols]).
replace_in_row([Col|RestCols], ColIndex, Elem, [Col|NewRestCols]) :-
    ColIndex > 1,
    NewColIndex is ColIndex - 1,
    replace_in_row(RestCols, NewColIndex, Elem, NewRestCols).

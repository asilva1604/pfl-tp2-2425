:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(random)).


% Initializes the game state for LOT.
% GameConfig: Contains additional configuration options if needed (e.g., future extensions).
% GameState: The initial state of the game.

initial_state((Size, 1), GameState) :-
    create_board(Size, Board),
    GameState = state(Board, white, 1).

initial_state((Size, 2), GameState) :-
    create_board(Size, Board),
    % Define the initial game state.
    GameState = state(Board, white, 2).

initial_state((Size, 3), GameState) :-
    create_board(Size, Board),
    % Define the initial game state.
    GameState = state(Board, white, 3).

initial_state((Size, 4), GameState) :-
    create_board(Size, Board),
    % Define the initial game state.
    GameState = state(Board, white, 4).
    
% Helper predicate to create an empty board of a given size.
create_board(Size, Board) :-
    length(Board, Size),                % Create a list of rows.
    maplist(create_row(Size), Board).   % Each row is a list of 'empty' cells.

% Helper predicate to create a row with 'empty' cells.
create_row(Size, Row) :-
    length(Row, Size),                  % Create a list of cells.
    maplist(=(empty), Row).             % Initialize all cells to 'empty'.

% Displays the current game state: the board and the current player.
display_game(state(Board, Player, _)) :-
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

move(state(Board, Player, Mode), (Row, Col), state(NewBoard, NextPlayer, Mode)) :-
    valid_position(Board, Row, Col),          % Ensure the position is valid.
    nth1(Row, Board, CurrentRow),            % Get the target row.
    nth1(Col, CurrentRow, empty),            % Ensure the target cell is empty.
    replace(Board, Row, Col, Player, TempBoard), % Update the board with the player's piece.
    check_lines(TempBoard, Player, Row, Col, TempBoard2, Mode), % Check for lines of three and handle them.
    (   game_over(state(TempBoard2, Player, Mode), Winner)
    ->  format('Game over! Winner: ~w~n', [Winner]), display_game(state(TempBoard2, Player, Mode)),!, fail
    ;   switch_player(Player, NextPlayer),       % Switch the player.
        NewBoard = TempBoard2                   % Set the new board state.
    ).

% Checks for lines of three or more consecutive pieces and handles them.
check_lines(Board, Player, Row, Col, NewBoard, 1) :-
    (   line_of_three(Board, Player, Row, Col, Line) ->
        handle_line(Board, Player, Line, NewBoard)
    ;   NewBoard = Board
    ).

check_lines(Board, Player, Row, Col, NewBoard, 2) :-
    (   line_of_three(Board, Player, Row, Col, Line) ->
        handle_line(Board, Player, Line, NewBoard)
    ;   NewBoard = Board
    ).

check_lines(Board, Player, Row, Col, NewBoard, 3) :-
    (   line_of_three(Board, Player, Row, Col, Line) ->
        handle_line(Board, Player, Line, NewBoard)
    ;   NewBoard = Board
    ).

check_lines(Board, Player, Row, Col, NewBoard, 4) :-
    (   line_of_three(Board, Player, Row, Col, Line) ->
        handle_line_ai(Board, Player, Line, NewBoard)
    ;   NewBoard = Board
    ).

% Handles a line of three pieces by asking the user which piece to keep as the stack.
handle_line(Board, Player, Line, NewBoard) :-
    format('Line of three found at coordinates: ~w~n', [Line]),
    writeln('Choose which piece to keep as the stack (1, 2, or 3): '),
    read(Choice),
    nth1(Choice, Line, (KeepRow, KeepCol)),
    delete(Line, (KeepRow, KeepCol), [(RemoveRow1, RemoveCol1), (RemoveRow2, RemoveCol2)]),
    replace(Board, RemoveRow1, RemoveCol1, empty, TempBoard1),
    replace(TempBoard1, RemoveRow2, RemoveCol2, empty, TempBoard2),
    replace(TempBoard2, KeepRow, KeepCol, stack(Player), NewBoard).

handle_line_ai(Board, Player, Line, NewBoard) :-
    random_member(Choice, Line),
    nth1(Choice, Line, (KeepRow, KeepCol)),
    delete(Line, (KeepRow, KeepCol), [(RemoveRow1, RemoveCol1), (RemoveRow2, RemoveCol2)]),
    replace(Board, RemoveRow1, RemoveCol1, empty, TempBoard1),
    replace(TempBoard1, RemoveRow2, RemoveCol2, empty, TempBoard2),
    replace(TempBoard2, KeepRow, KeepCol, stack(Player), NewBoard).

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
    find_adjacent_diagonal(Board, Player, Row, Col, -1, -1, UpLeftCells),
    find_adjacent_diagonal(Board, Player, Row, Col, -1, 1, UpRightCells),
    find_adjacent_diagonal(Board, Player, Row, Col, 1, -1, DownLeftCells),
    find_adjacent_diagonal(Board, Player, Row, Col, 1, 1, DownRightCells),
    append([UpLeftCells, UpRightCells, DownLeftCells, DownRightCells], AdjacentCells),
    member((AdjRow, AdjCol), AdjacentCells).

% Helper predicate to find adjacent cells in a specific direction.
find_adjacent_diagonal(Board, Player, Row, Col, RowDir, ColDir, AdjacentCells) :-
    NextRow is Row + RowDir,
    NextCol is Col + ColDir,
    (   NextRow > 0, NextCol > 0,
        nth1(NextRow, Board, RowList),
        nth1(NextCol, RowList, Player)
    ->  find_adjacent_diagonal(Board, Player, NextRow, NextCol, RowDir, ColDir, RestCells),
        AdjacentCells = [(NextRow, NextCol) | RestCells]
    ;   AdjacentCells = []
    ).

% Switches the player.
switch_player(white, black).
switch_player(black, white).

% Generates a list of all valid moves for the current game state.
valid_moves(state(Board, _, _), Moves) :-
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


% Determines if the game is over and declares the winner.
game_over(state(Board, _, _), Winner) :-
    (   winning_line(Board, stack(white)) -> Winner = white
    ;   winning_line(Board, stack(black)) -> Winner = black
    ;   board_full(Board) -> Winner = draw
    ;   fail % The game is not over yet.
    ).

% Checks if there is a winning line for the given stack type.
winning_line(Board, StackType) :-
(   row_win(Board, StackType)
;   column_win(Board, StackType)
;   diagonal_win(Board, StackType)
).

% Checks for a row win.
row_win(Board, StackType) :-
    member(Row, Board),
    sublist_of_three(Row, StackType).

% Checks for a column win by transposing the board and checking rows.
column_win(Board, StackType) :-
    transpose(Board, Transposed),
    row_win(Transposed, StackType).

% Checks for a diagonal win.
diagonal_win(Board, StackType) :-
    major_diagonal(Board, Diagonal),
    sublist_of_three(Diagonal, StackType).
diagonal_win(Board, StackType) :-
    minor_diagonal(Board, Diagonal),
    sublist_of_three(Diagonal, StackType).

% Helper: Finds a major diagonal.
major_diagonal(Board, Diagonal) :-
    findall(Cell, (nth1(Index, Board, Row), nth1(Index, Row, Cell)), Diagonal).

% Helper: Finds a minor diagonal.
minor_diagonal(Board, Diagonal) :-
    reverse(Board, Reversed),
    major_diagonal(Reversed, Diagonal).

% Helper: Checks if there is a sublist of three identical elements.
sublist_of_three(List, Elem) :-
    append(_, [Elem, Elem, Elem | _], List).

% Checks if the board is full (no empty cells).
board_full(Board) :-
    \+ (member(Row, Board), member(empty, Row)).

choose_move(state(Board, white, _), human, Move) :-
    writeln('Your colour is white. Enter your move:'),
    writeln('Enter your move row:'),
    read(Row),
    writeln('Enter your move column:'),
    read(Col),
    Move = (Row, Col).

choose_move(state(Board, black, _), human, Move) :-
    writeln('Your colour is black. Enter your move:'),
    writeln('Enter your move row:'),
    read(Row),
    writeln('Enter your move column:'),
    read(Col),
    Move = (Row, Col).

choose_move(state(Board, white, Mode), easy_ai, Move) :-
    valid_moves(state(Board, white, Mode), Moves),
    random_member(Move, Moves).

choose_move(state(Board, black, Mode), easy_ai, Move) :-
    valid_moves(state(Board, black, Mode), Moves),
    random_member(Move, Moves).

play :-
    writeln('Welcome to LOT! Enter the desired game mode'),
    writeln('1. Human vs Human'),
    writeln('2. Human vs AI'),
    writeln('3. AI vs Human'),
    writeln('4. AI vs AI'),
    read(GameMode),
    initial_state((7, GameMode), State),
    play(State).

play(state(Board, Player, 1)) :-
    display_game(state(Board, Player, 1)),
    choose_move(state(Board, Player, 1), human, Move),
    move(state(Board, Player, 1), Move, NewState),
    play(NewState).

play(state(Board, Player, 2)) :-
    writeln('Select the AI level:'),
    writeln('1. Easy'),
    writeln('2. Medium'),
    writeln('3. Hard'),
    read(AILevel),
    play(State, 2, human, AILevel).

play(state(Board, Player, 3)) :-
    writeln('Select the AI level:'),
    writeln('1. Easy'),
    writeln('2. Medium'),
    writeln('3. Hard'),
    read(AILevel),
    play(State, 2, ai, AILevel).

play(state(Board, Player, 3)) :-
    play(State, 4, ai1, 1).

play(State, 2, human, 1) :-
    display_game(State),
    choose_move(State, human, Move),
    move(State, Move, NewState),
    play(NewState, 2, ai, 1).

play(State, 2, ai, 1) :-
    display_game(State),
    choose_move(State, easy_ai, Move),
    move(State, Move, NewState),
    play(NewState, 2, human, 1).

play(State, 4, ai1, 1) :-
    display_game(State),
    choose_move(State, easy_ai, Move),
    move(State, Move, NewState),
    play(NewState, 4, ai2, 1).

play(State, 4, ai2, 1) :-
    display_game(State),
    choose_move(State, easy_ai, Move),
    move(State, Move, NewState),
    play(NewState, 4, ai1, 1).

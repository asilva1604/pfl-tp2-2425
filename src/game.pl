:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(random)).
:- consult('game_states.pl').


% initial_state(+GameConfig, -GameState)
% Initializes the game state for LOT.
% GameConfig: Contains additional configuration options if needed (e.g., future extensions).
% GameState: The initial state of the game.
initial_state(Size, GameState) :-
    create_board(Size, Board),
    GameState = state(Board, white).

% create_board(+Size, -Board)
% Helper predicate to create an empty board of a given size.
% Size: The size of the board (Size x Size).
% Board: The empty board.
create_board(Size, Board) :-
    length(Board, Size),                % Create a list of rows.
    maplist(create_row(Size), Board).   % Each row is a list of 'empty' cells.

% create_row(+Size, -Row)
% Helper predicate to create a row with 'empty' cells.
% Size: The size of the row.
% Row: The row with 'empty' cells.
create_row(Size, Row) :-
    length(Row, Size),                  % Create a list of cells.
    maplist(=(empty), Row).             % Initialize all cells to 'empty'.

% display_game(+GameState)
% Displays the current game state: the board and the current player.
% GameState : The current state of the game.
display_game((CurrPlayer, _, _), state(Board, Color)) :-
    write('Current player: '), writeln(CurrPlayer),
    write('Playing for color: '), writeln(Color),
    write('  1   2   3   4   5   6   7  '), nl, % Column headers
    display_rows(Board, 1).

% display_rows(+Board, +RowNum)
% Helper predicate to display rows with row numbers.
% Board: The current board state.
% RowNum: The current row number.
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

% symbol_for_cell(+Cell, -Symbol)
% Maps cell values to display symbols.
% Cell: The cell value.
% Symbol: The symbol to display.
symbol_for_cell(empty, '.  ').
symbol_for_cell(white, 'W  ').
symbol_for_cell(black, 'B  ').
symbol_for_cell(stack(white), 'WW ').
symbol_for_cell(stack(black), 'BB ').

% writeln(+X)
% Helper predicate to write a line to the console.
% X: The value to write.
writeln(X) :-
    write(X),
    nl.

% move(+GameState, +Move, -NewGameState)
% Executes a move in the game.
% GameState: The current state of the game.
% Move: The move to be executed.
% NewGameState: The new state of the game after the move.
move(state(Board, Player), ((Row, Col), no_stack, no_pie_rule), state(NewBoard, NextPlayer)) :-
    valid_position(Board, Row, Col),  % check if position is inside board and empty
    replace(Board, Row, Col, Player, NewBoard), % board with added piece in NewBoard
    findall(1, valid_stack(NewBoard, Player, _), StacksFound),
    length(StacksFound, 0),         % ensure there are no stacks
    switch_player(Player, NextPlayer).

% apply pie rule and place move
move(state(Board, Player), ((Row, Col), no_stack, pie_rule), state(NewBoard, Player)) :-
    pie_rulable(Board),     % check if pie_rule can be applied
    switch_player(Player, SwappedPlayer),       % switch color
    valid_position(Board, Row, Col),  % check if position is inside board and empty
    replace(Board, Row, Col, SwappedPlayer, NewBoard). % place piece as SwappedPlayer

% place and stack move
move(state(Board, Player), ((Row, Col), StackMove, no_pie_rule), state(NewBoard, NextPlayer)) :-
    valid_position(Board, Row, Col),  % check if position is inside board and empty
    replace(Board, Row, Col, Player, PieceAddedBoard), % board with added piece in PieceAddedBoard
    valid_stack(PieceAddedBoard, Player, StackMove),
    build_stack(PieceAddedBoard, Player, StackMove, NewBoard),
    switch_player(Player, NextPlayer).

% build_stack(+Board, +Player, +StackMove, -NewBoard)
% Builds a stack for the given player.
% Board: The current board state.
% Player: The player building the stack.
% StackMove: The stack move to be executed
% NewBoard: The new board state after building the stack.
build_stack(Board, Player, ((SRow, SCol), (R1Row, R1Col), (R2Row, R2Col)), NewBoard) :-
    replace(Board, SRow, SCol, stack(Player), TempBoard1),
    replace(TempBoard1, R1Row, R1Col, empty, TempBoard2),
    replace(TempBoard2, R2Row, R2Col, empty, NewBoard).

% valid_stack(+Board, +Player, -StackMove)
% Checks if a stack move is valid for the given player.
% Board: The current board state.
% Player: The player building the stack.
% StackMove: The stack move to be executed.
valid_stack(Board, Player, ((SRow, SCol), (R1Row, R1Col), (R2Row, R2Col))) :-
    % check if all positions are occupied by single pieces of Player color
    piece_in_pos(Board, Player, (SRow, SCol)),
    piece_in_pos(Board, Player, (R1Row, R1Col)),
    piece_in_pos(Board, Player, (R2Row, R2Col)),

    % check if positions are in a line
    continuous_line((SRow, SCol), (R1Row, R1Col), (R2Row, R2Col)).

% continuous_line(+Pos1, +Pos2, +Pos3)
% helper predicate to check if three positions form a continuous line
% Pos1, Pos2, Pos3: The positions to check.
continuous_line((X1, Y1), (X2, Y2), (X3, Y3)) :-
    % sort the positions to help with the continuity check
    sort([(X1, Y1), (X2, Y2), (X3, Y3)], [(A1, B1), (A2, B2), (A3, B3)]),
    % check alignment and continuity
    (
        % horizontal line
        A1 = A2, A2 = A3, B2 - B1 =:= 1, B3 - B2 =:= 1;
        % vertical line
        B1 = B2, B2 = B3, A2 - A1 =:= 1, A3 - A2 =:= 1;
        % diagonal line (top-left to bottom-right)
        A2 - A1 =:= 1, B2 - B1 =:= 1, A3 - A2 =:= 1, B3 - B2 =:= 1;
        % diagonal line (top-right to bottom-left)
        A2 - A1 =:= 1, B1 - B2 =:= 1, A3 - A2 =:= 1, B2 - B3 =:= 1
    ).


% piece_in_pos(+Board, +Piece, +Position)
% Piece is piece at (Row, Col) position of Board
% Board: The current board state.
% Piece: The piece to check for.
% Position: The position to check.
piece_in_pos(Board, Piece, (Row, Col)) :-
    nth1(Row, Board, CurrentRow),
    nth1(Col, CurrentRow, Piece).

% check_lines(+Board, +Player, +Row, +Col, -NewBoard, +StackMove)
% Checks for lines of three or more consecutive pieces and handles them.
% Board: The current board state.
% Player: The player placing the piece.
% Row: The row of the placed piece.
% Col: The column of the placed piece.
% NewBoard: The new board state after handling the lines.
% StackMove: The stack move to be executed
check_lines(Board, Player, Row, Col, NewBoard, 4) :-
    !,
    (   line_of_three(Board, Player, Row, Col, Line) ->
        handle_line_ai(Board, Player, Line, NewBoard)
    ;   NewBoard = Board
    ).

check_lines(Board, Player, Row, Col, StackMove) :-
    (   line_of_three(Board, Player, Row, Col, Line) ->
        handle_line(Board, Player, Line, StackMove)
    ;   StackMove = no_stack
    ).

check_lines(Board, Player, Row, Col, StackMove) :-
    line_of_three(Board, Player, Row, Col, Line), !,
    handle_line(Board, Player, Line, StackMove).

check_lines(_, _, _, _, no_stack).



% handle_line(+Board, +Player, +Line, -NewBoard)
% Handles a line of three or more pieces by asking the user which piece to keep as the stack.
% Board: The current board state.
% Player: The player placing the piece.
% Line: The line of three or more pieces.
% NewBoard: The new board state after handling the line.
handle_line(Board, Player, Line, NewBoard) :-
    format('Line of three or more found at coordinates: ~w~n', [Line]),
    writeln('Choose which piece to keep as the stack (1, 2, 3, etc): '),
    read(Choice),
    nth1(Choice, Line, (KeepRow, KeepCol)),
    delete(Line, (KeepRow, KeepCol), RemainingLine),
    remove_pieces(RemainingLine, Board, TempBoard),
    replace(TempBoard, KeepRow, KeepCol, stack(Player), NewBoard).

% handle_line_ai(+Board, +Player, +Line, -NewBoard)
% Handles a line of three or more pieces automatically for AI by keeping the middle piece as the stack.
% Board: The current board state.
% Player: The player placing the piece.
% Line: The line of three or more pieces.
% NewBoard: The new board state after handling the line.
handle_line_ai(Board, Player, Line, NewBoard) :-
    random(1, 3, Index),
    nth1(Index, Line, (KeepRow, KeepCol)),
    delete(Line, (KeepRow, KeepCol), RemainingLine),
    remove_pieces(RemainingLine, Board, TempBoard),
    replace(TempBoard, KeepRow, KeepCol, stack(Player), NewBoard).

% remove_pieces(+Pieces, +Board, -NewBoard)
% Removes pieces from the board.
% Pieces: The list of pieces to remove.
% Board: The current board state.
% NewBoard: The new board state after removing the pieces.
remove_pieces([], Board, Board).
remove_pieces([(Row, Col) | Rest], Board, NewBoard) :-
    replace(Board, Row, Col, empty, TempBoard),
    remove_pieces(Rest, TempBoard, NewBoard).

% line_of_three(+Board, +Player, +Row, +Col, -Line)
% Checks if placing a piece creates a line of three or more consecutive pieces.
% Board: The current board state.
% Player: The player placing the piece.
% Row: The row of the placed piece.
% Col: The column of the placed piece.
% Line: The line of three or more consecutive pieces.
line_of_three(Board, Player, Row, Col, Line) :-
    (   horizontal_line(Board, Player, Row, Col, Line);
        vertical_line(Board, Player, Row, Col, Line);
        diagonal_line(Board, Player, Row, Col, Line)
    ).

% adjacent_horizontal(+Board, +Player, +Row, +Col, -AdjCol)
% Finds adjacent cells horizontally.
% Board: The current board state.
% Player: The player placing the piece.
% Row: The row of the placed piece.
% Col: The column of the placed piece.
% AdjCol: The adjacent column.
adjacent_horizontal(Board, Player, Row, Col, AdjCol) :-
    find_adjacent_horizontal(Board, Player, Row, Col, -1, LeftCells),
    find_adjacent_horizontal(Board, Player, Row, Col, 1, RightCells),
    append(LeftCells, RightCells, AdjacentCells),
    member(AdjCol, AdjacentCells).

% find_adjacent_horizontal(+Board, +Player, +Row, +Col, +Direction, -AdjacentCells)
% Helper predicate to find adjacent cells in a specific direction.
% Board: The current board state.
% Player: The player placing the piece.
% Row: The row of the placed piece.
% Col: The column of the placed piece.
% Direction: The direction to search (-1 for left, 1 for right).
% AdjacentCells: The list of adjacent cells.
find_adjacent_horizontal(Board, Player, Row, Col, Direction, AdjacentCells) :-
    NextCol is Col + Direction,
    (   NextCol > 0,
        nth1(Row, Board, RowList),
        nth1(NextCol, RowList, Player)
    ->  find_adjacent_horizontal(Board, Player, Row, NextCol, Direction, RestCells),
        AdjacentCells = [NextCol | RestCells]
    ;   AdjacentCells = []
    ).

% horizontal_line(+Board, +Player, +Row, +Col, -Line)
% Checks for a horizontal line of three or more consecutive pieces.
% Board: The current board state.
% Player: The player placing the piece.
% Row: The row of the placed piece.
% Col: The column of the placed piece.
% Line: The horizontal line of three or more consecutive pieces.
horizontal_line(Board, Player, Row, Col, Line) :-
    findall((Row, C), (adjacent_horizontal(Board, Player, Row, Col, C)), AdjacentCells),
    length(AdjacentCells, Length),
    Length >= 2,
    Line = [(Row, Col) | AdjacentCells].

% vertical_line(+Board, +Player, +Row, +Col, -Line)
% Checks for a vertical line of three or more consecutive pieces.
% Board: The current board state.
% Player: The player placing the piece.
% Row: The row of the placed piece.
% Col: The column of the placed piece.
% Line: The vertical line of three or more consecutive pieces.
vertical_line(Board, Player, Row, Col, Line) :-
    findall((R, Col), (adjacent_vertical(Board, Player, Row, Col, R)), AdjacentCells),
    length(AdjacentCells, Length),
    Length >= 2,
    Line = [(Row, Col) | AdjacentCells].

% adjacent_vertical(+Board, +Player, +Row, +Col, -AdjRow)
% Finds adjacent cells vertically.
% Board: The current board state.
% Player: The player placing the piece.
% Row: The row of the placed piece.
% Col: The column of the placed piece.
% AdjRow: The adjacent row.
adjacent_vertical(Board, Player, Row, Col, AdjRow) :-
    find_adjacent_vertical(Board, Player, Row, Col, -1, UpCells),
    find_adjacent_vertical(Board, Player, Row, Col, 1, DownCells),
    append(UpCells, DownCells, AdjacentCells),
    member(AdjRow, AdjacentCells).

% find_adjacent_vertical(+Board, +Player, +Row, +Col, +Direction, -AdjacentCells)
% Helper predicate to find adjacent cells in a specific direction.
% Board: The current board state.
% Player: The player placing the piece.
% Row: The row of the placed piece.
% Col: The column of the placed piece.
% Direction: The direction to search (-1 for up, 1 for down).
% AdjacentCells: The list of adjacent cells.
find_adjacent_vertical(Board, Player, Row, Col, Direction, AdjacentCells) :-
    NextRow is Row + Direction,
    (   NextRow > 0,
        nth1(NextRow, Board, RowList),
        nth1(Col, RowList, Player)
    ->  find_adjacent_vertical(Board, Player, NextRow, Col, Direction, RestCells),
        AdjacentCells = [NextRow | RestCells]
    ;   AdjacentCells = []
    ).

% adjacent_diagonal(+Board, +Player, +Row, +Col, -AdjRow, -AdjCol)
% Finds adjacent cells diagonally.
% Board: The current board state.
% Player: The player placing the piece.
% Row: The row of the placed piece.
% Col: The column of the placed piece.
% AdjRow: The adjacent row.
% AdjCol: The adjacent column.
adjacent_diagonal(Board, Player, Row, Col, AdjRow, AdjCol) :-
    find_adjacent_diagonal(Board, Player, Row, Col, -1, -1, UpLeftCells),
    find_adjacent_diagonal(Board, Player, Row, Col, -1, 1, UpRightCells),
    find_adjacent_diagonal(Board, Player, Row, Col, 1, -1, DownLeftCells),
    find_adjacent_diagonal(Board, Player, Row, Col, 1, 1, DownRightCells),
    append([UpLeftCells, UpRightCells, DownLeftCells, DownRightCells], AdjacentCells),
    member((AdjRow, AdjCol), AdjacentCells).

% diagonal_line(+Board, +Player, +Row, +Col, -Line)
% Checks for a diagonal line of three or more consecutive pieces
% Board: The current board state.
% Player: The player placing the piece.
% Row: The row of the placed piece.
% Col: The column of the placed piece.
% Line: The diagonal line of three or more consecutive pieces.
diagonal_line(Board, Player, Row, Col, Line) :-
    findall((R, C), (
        adjacent_diagonal_2(Board, Player, Row, Col, R, C)
    ), AdjacentCells),
    length(AdjacentCells, Length),
    Length >= 2,
    are_consecutive_diagonal([(Row, Col)|AdjacentCells]),
    Line = [(Row, Col)|AdjacentCells].

% adjacent_diagonal_2(+Board, +Player, +Row, +Col, -AdjRow, -AdjCol)
% Modified adjacent_diagonal to ensure consecutiveness
% Board: The current board state.
% Player: The player placing the piece.
% Row: The row of the placed piece.
% Col: The column of the placed piece.
% AdjRow: The adjacent row.
% AdjCol: The adjacent column.
adjacent_diagonal_2(Board, Player, Row, Col, AdjRow, AdjCol) :-
    find_adjacent_diagonal(Board, Player, Row, Col, -1, -1, UpLeftCells),
    find_adjacent_diagonal(Board, Player, Row, Col, -1, 1, UpRightCells),
    find_adjacent_diagonal(Board, Player, Row, Col, 1, -1, DownLeftCells),
    find_adjacent_diagonal(Board, Player, Row, Col, 1, 1, DownRightCells),
    append([UpLeftCells, UpRightCells, DownLeftCells, DownRightCells], AdjacentCells),
    member((AdjRow, AdjCol), AdjacentCells).

% are_consecutive_diagonal(+Points)
% Check if points form a consecutive diagonal line
% Points: The list of points to check.
are_consecutive_diagonal(Points) :-
    sort_points(Points, Sorted),  % Sort points by row
    consecutive_points(Sorted).

% sort_points(+Points, -Sorted)
% Sort points by row number
% Points: The list of points to sort.
% Sorted: The sorted list of points.
sort_points(Points, Sorted) :-
    % Convert points to row-keyed terms for sorting
    findall(Key-(R,C), (
        member((R,C), Points),
        Key = R
    ), KeyPoints),
    keysort(KeyPoints, SortedKeyPoints),
    % Extract just the points back out
    findall((R,C), 
        member(_-(R,C), SortedKeyPoints),
        Sorted).

% consecutive_points(+Points)
% Check if points form a consecutive line
% Points: The list of points to check.
consecutive_points([]).
consecutive_points([_]).
consecutive_points([(R1, C1), (R2, C2)|Rest]) :-
    abs(R2 - R1) =:= 1,
    abs(C2 - C1) =:= 1,
    consecutive_points([(R2, C2)|Rest]).

% find_adjacent_diagonal(+Board, +Player, +Row, +Col, +RowDir, +ColDir, -AdjacentCells)
% Finds adjacent cells diagonally in a specific direction.
% Board: The current board state.
% Player: The player placing the piece.
% Row: The row of the placed piece.
% Col: The column of the placed piece.
% RowDir: The direction to search for rows (-1 for up, 1 for down).
% ColDir: The direction to search for columns (-1 for left, 1 for right).
% AdjacentCells: The list of adjacent cells.
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

% switch_player(+Player, -NextPlayer)
% Switches the player.
% Player: The current player.
% NextPlayer: The next player.
switch_player(white, black).
switch_player(black, white).

% valid_moves(+GameState, -ListOfMoves)
% Finds all valid moves for the current player.
% GameState: The current state of the game.
% ListOfMoves: The list of valid moves for the current player.
valid_moves(state(Board, Player), Moves) :-
    findall(Move , move(state(Board, Player), Move, _), Moves).

% valid_position(+Board, +Row, +Col)
% Ensures the position (Row, Col) is valid (empty and within bounds).
% Board: The current board state.
% Row: The row of the position.
% Col: The column of the position.
valid_position(Board, Row, Col) :-
    length(Board, Size),         % Get the board size.
    between(1, Size, Row),       % Ensure the row is within bounds.
    between(1, Size, Col),       % Ensure the column is within bounds.
    nth1(Row, Board, CurrentRow),
    nth1(Col, CurrentRow, empty). % Ensure the position is empty.

% replace(+Board, +RowIndex, +Col, +Elem, -NewBoard)
% Replaces the element at (Row, Col) in the board with a new element.
% Board: The current board state.
% RowIndex: The row index of the element to replace.
% Col: The column index of the element to replace.
% Elem: The new element to place.
% NewBoard: The new board state after replacing the element.
replace([Row|RestRows], 1, Col, Elem, [NewRow|RestRows]) :-
    replace_in_row(Row, Col, Elem, NewRow).
replace([Row|RestRows], RowIndex, Col, Elem, [Row|NewRestRows]) :-
    RowIndex > 1,
    NewRowIndex is RowIndex - 1,
    replace(RestRows, NewRowIndex, Col, Elem, NewRestRows).

% replace_in_row(+Row, +ColIndex, +Elem, -NewRow)
% Replaces the element at Col in the row with a new element.
% Row: The current row.
% ColIndex: The column index of the element to replace.
% Elem: The new element to place.
% NewRow: The new row after replacing the element.
replace_in_row([_|RestCols], 1, Elem, [Elem|RestCols]).
replace_in_row([Col|RestCols], ColIndex, Elem, [Col|NewRestCols]) :-
    ColIndex > 1,
    NewColIndex is ColIndex - 1,
    replace_in_row(RestCols, NewColIndex, Elem, NewRestCols).


% game_over(+GameState, -Winner)
% Determines if the game is over and declares the winner.
% GameState: The current state of the game.
% Winner: The winner of the game (white, black, or draw).
game_over(state(Board, _), Winner) :-
    (   winning_line(Board, stack(white)) -> Winner = white
    ;   winning_line(Board, stack(black)) -> Winner = black
    ;   board_full(Board) -> Winner = draw
    ;   fail % The game is not over yet.
    ).

% winning_line(+Board, +StackType)
% Checks if there is a winning line for the given stack type.
% Board: The current board state.
% StackType: The stack type to check for (stack(white) or stack(black)).
winning_line(Board, StackType) :-
(   row_win(Board, StackType)
;   column_win(Board, StackType)
;   diagonal_win(Board, StackType)
).

% row_win(+Board, +StackType)
% Checks for a row win.
% Board: The current board state.
% StackType: The stack type to check for (stack(white) or stack(black)).
row_win(Board, StackType) :-
    member(Row, Board),
    sublist_of_three(Row, StackType).

% column_win(+Board, +StackType)
% Checks for a column win by transposing the board and checking rows.
% Board: The current board state.
% StackType: The stack type to check for (stack(white) or stack(black)).
column_win(Board, StackType) :-
    transpose(Board, Transposed),
    row_win(Transposed, StackType).

% diagonal_win(+Board, +StackType)
% Checks for a diagonal win.
% Board: The current board state.
% StackType: The stack type to check for (stack(white) or stack(black)).
diagonal_win(Board, StackType) :-
    major_diagonal(Board, Diagonal),
    sublist_of_three(Diagonal, StackType).
diagonal_win(Board, StackType) :-
    minor_diagonal(Board, Diagonal),
    sublist_of_three(Diagonal, StackType).

% major_diagonal(+Board, -Diagonal)
% Helper: Finds a major diagonal.
% Board: The current board state.
% Diagonal: The major diagonal.
major_diagonal(Board, Diagonal) :-
    findall(Cell, (nth1(Index, Board, Row), nth1(Index, Row, Cell)), Diagonal).

% minor_diagonal(+Board, -Diagonal)
% Helper: Finds a minor diagonal.
% Board: The current board state.
% Diagonal: The minor diagonal.
minor_diagonal(Board, Diagonal) :-
    reverse(Board, Reversed),
    major_diagonal(Reversed, Diagonal).

% sublist_of_three(+List, +Elem)
% Helper: Checks if there is a sublist of three identical elements.
% List: The list to check.
% Elem: The element to check for.
sublist_of_three(List, Elem) :-
    append(_, [Elem, Elem, Elem | _], List).

% board_full(+Board)
% Checks if the board is full (no empty cells).
% Board: The current board state.
board_full(Board) :-
    \+ (member(Row, Board), member(empty, Row)).

% get_gamemode(-Gamemode)
% Asks the user for the desired game mode.
% Gamemode: The selected game mode.
get_gamemode(Gamemode) :-
    writeln('Welcome to LOT! Enter the desired game mode'),
    writeln('1. Human vs Human'),
    writeln('2. Human vs AI'),
    writeln('3. AI vs Human'),
    writeln('4. AI vs AI'),
    read(Number),
    gamemode_number(Number, Gamemode).

% gamemode_number(+Number, -Gamemode)
% Maps the selected number to the corresponding game mode.
% Number: The selected number.
% Gamemode: The corresponding game mode.
gamemode_number(1, h-h).
gamemode_number(2, h-pc).
gamemode_number(3, pc-h).
gamemode_number(4, pc-pc).

% get_settings(+Gamemode, -MatchState)
% Asks the user for the desired settings for the selected game mode.
% Gamemode: The selected game mode.
% MatchState: The settings for the selected game mode.
get_settings(First-Second, (player1, PType1, PType2)) :-
    get_settings(First, PType1),
    get_settings(Second, PType2).

get_settings(h, human).

get_settings(pc, Difficulty) :-
    writeln('Select the AI level:'),
    writeln('1. Easy'),
    writeln('2. Medium'),
    writeln('3. Hard'),
    read(Number),
    difficulty_number(Number, Difficulty).

% difficulty_number(+Number, -Difficulty)
% Maps the selected number to the corresponding AI difficulty.
% Number: The selected number.
% Difficulty: The corresponding AI difficulty.
difficulty_number(1, easy_ai).
difficulty_number(2, medium_ai).
difficulty_number(3, hard_ai).

% display_winner(+MatchState, +Winner)
% Displays the winner of the game.
% MatchState: The current state of the match.
% Winner: The winner of the game.
display_winner((Player, _, _), WinnerColor) :-
    write(Player), write(' won as color '), write(WinnerColor), writeln('!!!').

% play
% Starts the game.
play :-
    get_gamemode(Gamemode),
    initial_state(7, GameState),
    get_settings(Gamemode, MatchState),
    game_loop(MatchState, GameState, Winner),
    display_winner(MatchState, Winner).

% game_loop(+MatchState, +GameState, -Winner)
% Main game loop.
% MatchState: The current state of the match.
% GameState: The current state of the game.
% Winner: The winner of the game.
game_loop(_, GameState, Winner) :-
    game_over(GameState, Winner),
    !.

game_loop((player1, PType1, PType2), GameState, Winner) :- 
    display_game((player1, _, _), GameState),
    \+ game_over(GameState, Winner),
    choose_move(GameState, PType1, Move),
    move(GameState, Move, NewGameState),
    game_loop((player2, PType1, PType2), NewGameState, Winner).

game_loop((player2, PType1, PType2), GameState, Winner) :- 
    display_game((player2, _, _), GameState),
    \+ game_over(GameState, Winner),
    choose_move(GameState, PType2, Move),
    move(GameState, Move, NewGameState),
    game_loop((player1, PType1, PType2), NewGameState, Winner).

% pie_rulable(+Board)
% Checks if the pie rule can be applied.
% Board: The current board state.
pie_rulable(Board) :- one_piece(Board).

% one_piece(+Board)
% Checks if the board has only one piece.
% Board: The current board state.
one_piece([Head|Tail]) :-      % applies for both Row-Board and Element-Row pairs
    one_piece(Head),
    no_piece(Tail).
one_piece([Head|Tail]) :-
    no_piece(Head),
    one_piece(Tail).

one_piece(white).
one_piece(black).

% no_piece(+Board)
% Checks if the board has no pieces.
% Board: The current board state.
no_piece([Head|Tail]) :-
    no_piece(Head),
    no_piece(Tail).
no_piece(empty).
no_piece([]).

% if board results in game win, assign it max value
value(Board, Player, 999) :-
    game_over(state(Board, Player), Player).

% losing must be worse than win
value(Board, Player, -999) :-
    switch_player(Player, Opponent),
    game_over(state(Board, Player), Opponent).

% favorable conditions heuristics
% > any single piece on the board - 0 pts - as both players are always obliged to place one
% > 2 pieces (either single or stack) in an uninterrupted line - 1 pt
% > 1 stack - 4 pts - so as to be larger than the maximum pts one board can achieve greater than the previous by placing a piece that doesn't make a stack
% > (?) 2 stacks in an uninterrupted line - 2 pts

% NOTE: first condition is only counting regarding lines of 2 single pieces
% NOTE: may need some weight towards placing single pieces near stacks; otherwise, level 2 AI may be a bit random (this might be fixed by addressing previous note)
% value(+GameState, +Player, -Value)
% Calculates the value of the game state for the given player.
% GameState: The current state of the game.
% Player: The player for whom the value is calculated.
% Value: The value of the game state for the given player.
value(Board, Player, Value) :-
    count_lines_of_two(Board, Player, LineOfTwoSingleCount),
    count_stacks(Board, Player, StackCount),
    count_lines_of_two(Board, stack(Player), LineOfTwoStackCount),
    Value is LineOfTwoSingleCount + StackCount*4 + LineOfTwoStackCount*2.

% counts lines of 2 uninterrupted (by opposite color) pieces (stack or single) of Player color
count_lines_of_two(Board, Player, Count) :-
    findall(1, (nth1(_, Board, CurrentRow),  nth1(Col, CurrentRow, empty), line_of_three(Board, Player, CurrentRow, Col, _)), FavorableCells),
    length(FavorableCells, Count).

count_stacks(Board, Player, Count) :-
    findall(1, (nth1(_, Board, CurrentRow),  nth1(_, CurrentRow, stack(Player))), Stacks),
    length(Stacks, Count).

% choose_move(+GameState, +Level, -Move)
% Chooses a move for the given level of AI, or for a human.
% GameState: The current state of the game.
% Level: The level of the AI (easy_ai, medium_ai, hard_ai), or human.
% Move: The chosen move.
choose_move(state(Board, Player), easy_ai, Move) :-
    valid_moves(state(Board, Player), Moves),
    random_member(Move, Moves).


% medium -> best value achievable in 1 move
choose_move(state(Board, Player), medium_ai, BestMove) :-
    valid_moves(state(Board, Player), _),
    setof((Value, Move), NewState^(move(state(Board, Player), Move, NewState), value(NewState, Player, Value)), ValueMoveMap),
    last(ValueMoveMap, (_, BestMove)).  % last has the highest value since setof sorts elements

% hard -> best value achievable in 2 moves, while trying to predict opponent's next move (minimax)
choose_move(state(Board, Player), hard_ai, _) :-
    valid_moves(state(Board, Player), _),
    switch_player(Player, _),
    findall((MyMove, OpMove), NewState^(move(state(Board,Player), MyMove, NewState), choose_move(NewState, medium_ai, OpMove)), _).

choose_move(state(_, _), human, Move) :-
    ask_pie_rule(PieRule),
    ask_place(Position),
    ask_build_stack(StackMove),
    Move = (Position, StackMove, PieRule).
    
% ask_place(-Position)
% Asks the user for the position to place a piece.
% Position: The position to place a piece.
ask_place(Position) :-
    writeln('Enter your move row:'),
    read(Row),
    writeln('Enter your move column:'),
    read(Col),
    Position = (Row, Col).

% ask_pie_rule(-PieRule)
% Asks the user if they want to apply the pie rule.
% PieRule: The pie rule answer.
ask_pie_rule(PieRule) :-
    writeln('Would you like to apply the pie rule? (y/n)'),
    read(Answer),
    pie_rule_answer(Answer, PieRule).

% pie_rule_answer(+Answer, -PieRule)
% Maps the user answer to the pie rule.
% Answer: The user answer.
% PieRule: The pie rule.
pie_rule_answer(y, pie_rule).
pie_rule_answer(n, no_pie_rule).

% ask_build_stack(-StackMove)
% Asks the user for the stack move.
% StackMove: The stack move.
ask_build_stack(StackMove) :-
    writeln('Would you like to build a stack? (y/n)'),
    read(Answer),
    ask_stack_move(Answer, StackMove).

% ask_stack_move(+Answer, -StackMove)
% Maps the user answer to the stack move.
% Answer: The user answer.
% StackMove: The stack move.
ask_stack_move(n, no_stack).

ask_stack_move(y, StackMove) :-
    writeln('Enter your stack row:'),
    read(SRow),
    writeln('Enter your stack column:'),
    read(SCol),
    SPos = (SRow, SCol),

    writeln('Enter your first to remove piece row:'),
    read(R1Row),
    writeln('Enter your first to remove piece column:'),
    read(R1Col),
    R1Pos = (R1Row, R1Col),

    writeln('Enter your second to remove piece row:'),
    read(R2Row),
    writeln('Enter your second to remove piece column:'),
    read(R2Col),
    R2Pos = (R2Row, R2Col),

    StackMove = (SPos, R1Pos, R2Pos).

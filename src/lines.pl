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

% winning_line(+Board, +StackType)
% Checks if there is a winning line for the given stack type.
% Board: The current board state.
% StackType: The stack type to check for (stack(white) or stack(black)).
winning_line(Board, StackType) :-
(   row_win(Board, StackType)
;   column_win(Board, StackType)
;   diagonal_win(Board, StackType)
).

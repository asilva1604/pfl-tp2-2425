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

% switch_player(+Player, -NextPlayer)
% Switches the player.
% Player: The current player.
% NextPlayer: The next player.
switch_player(white, black).
switch_player(black, white).

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

% counts lines of 2 uninterrupted (by opposite color) pieces (stack or single) of Player color
count_lines_of_two(Board, Player, Count) :-
    findall(1, (nth1(_, Board, CurrentRow),  nth1(Col, CurrentRow, empty), line_of_three(Board, Player, CurrentRow, Col, _)), FavorableCells),
    length(FavorableCells, Count).

count_stacks(Board, Player, Count) :-
    findall(1, (nth1(_, Board, CurrentRow),  nth1(_, CurrentRow, stack(Player))), Stacks),
    length(Stacks, Count).

% remove_pieces(+Pieces, +Board, -NewBoard)
% Removes pieces from the board.
% Pieces: The list of pieces to remove.
% Board: The current board state.
% NewBoard: The new board state after removing the pieces.
remove_pieces([], Board, Board).
remove_pieces([(Row, Col) | Rest], Board, NewBoard) :-
    replace(Board, Row, Col, empty, TempBoard),
    remove_pieces(Rest, TempBoard, NewBoard).


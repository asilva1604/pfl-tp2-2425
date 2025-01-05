:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(random)).


% Initializes the game state for LOT.
% GameConfig: Contains additional configuration options if needed (e.g., future extensions).
% GameState: The initial state of the game.

initial_state(Size, GameState) :-
    create_board(Size, Board),
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
display_game((CurrPlayer, _, _), state(Board, Color)) :-
    write('Current player: '), writeln(CurrPlayer),
    write('Playing for color: '), writeln(Color),
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

/*
state(Board, Player, Mode):
    - Board: current board state;
    - Player: player that should play make the move;
    - Mode: H-H, H-AI, AI-H or AI-AI.
(Row, Col): position where piece is to be added.

*/

% simple place move
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

build_stack(Board, Player, ((SRow, SCol), (R1Row, R1Col), (R2Row, R2Col)), NewBoard) :-
    replace(Board, SRow, SCol, stack(Player), TempBoard1),
    replace(TempBoard1, R1Row, R1Col, empty, TempBoard2),
    replace(TempBoard2, R2Row, R2Col, empty, NewBoard).

/*
valid_stack(Board, Player, ((SRow, SCol), (R1Row, R1Col), (R2Row, R2Col))) :-
    % Ensure the chosen piece is part of a line of three.
    line_of_three(Board, Player, SRow, SCol, Line),
    % Ensure the chosen piece is one of the three pieces in the line.
    member((SRow, SCol), Line),
    % Ensure the pieces to be removed are also part of the line.
    member((R1Row, R1Col), Line),
    member((R2Row, R2Col), Line),
    % Ensure the chosen piece is not the same as the pieces to be removed.
    (SRow, SCol) \= (R1Row, R1Col),
    (SRow, SCol) \= (R2Row, R2Col),
    (R1Row, R1Col) \= (R2Row, R2Col).
*/

valid_stack(Board, Player, ((SRow, SCol), (R1Row, R1Col), (R2Row, R2Col))) :-
    % check if all positions are occupied by single pieces of Player color
    piece_in_pos(Board, Player, (SRow, SCol)),
    piece_in_pos(Board, Player, (R1Row, R1Col)),
    piece_in_pos(Board, Player, (R2Row, R2Col)),

    % check if positions are in a line
    continuous_line((SRow, SCol), (R1Row, R1Col), (R2Row, R2Col)).


% helper predicate to check if three positions form a continuous line
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



% Piece is piece at (Row, Col) position of Board
piece_in_pos(Board, Piece, (Row, Col)) :-
    nth1(Row, Board, CurrentRow),
    nth1(Col, CurrentRow, Piece).

/*---- old moves ------

move(state(Board, Player, Mode), (Row, Col), state(NewBoard, NextPlayer, Mode)) :-
    pie_rulable(Board),
    writeln('Do you want to change color? (y/n)'),
    read(Response),
    (Response = 'y' -> switch_player(Player, SwappedPlayer)),
    valid_position(Board, Row, Col),          % Ensure the position is valid.
    nth1(Row, Board, CurrentRow),            % Get the target row.
    nth1(Col, CurrentRow, empty),            % Ensure the target cell is empty.
    replace(Board, Row, Col, SwappedPlayer, TempBoard), % Update the board with the player's piece.
    check_lines(TempBoard, Player, Row, Col, TempBoard2, Mode), % Check for lines of three and handle them.
    (   game_over(state(TempBoard2, Player, Mode), Winner)
    ->  format('Game over! Winner: ~w~n', [Winner]), display_game(state(TempBoard2, Player, Mode)),!, halt
    ;   switch_player(SwappedPlayer, NextPlayer), NewBoard = TempBoard2                   % Set the new board state.
    ).

move(state(Board, Player, Mode), (Row, Col), state(NewBoard, NextPlayer, Mode), ai) :-
    pie_rulable(Board),
    switch_player(Player, SwappedPlayer),
    valid_position(Board, Row, Col),          % Ensure the position is valid.
    nth1(Row, Board, CurrentRow),            % Get the target row.
    nth1(Col, CurrentRow, empty),            % Ensure the target cell is empty.
    replace(Board, Row, Col, SwappedPlayer, TempBoard), % Update the board with the player's piece.
    check_lines(TempBoard, Player, Row, Col, TempBoard2, Mode), % Check for lines of three and handle them.
    (   game_over(state(TempBoard2, Player, Mode), Winner)
    ->  format('Game over! Winner: ~w~n', [Winner]), display_game(state(TempBoard2, Player, Mode)),!, halt
    ;   switch_player(SwappedPlayer, NextPlayer), NewBoard = TempBoard2                   % Set the new board state.
    ).

move(state(Board, Player, Mode), (Row, Col), state(NewBoard, NextPlayer, Mode)) :-
    valid_position(Board, Row, Col),          % Ensure the position is valid.
    nth1(Row, Board, CurrentRow),            % Get the target row.
    nth1(Col, CurrentRow, empty),            % Ensure the target cell is empty.
    replace(Board, Row, Col, Player, TempBoard), % Update the board with the player's piece.
    check_lines(TempBoard, Player, Row, Col, TempBoard2, Mode), % Check for lines of three and handle them.
    (   game_over(state(TempBoard2, Player, Mode), Winner)
    ->  format('Game over! Winner: ~w~n', [Winner]), display_game(state(TempBoard2, Player, Mode)),!, halt
    ;   switch_player(Player, NextPlayer), NewBoard = TempBoard2                   % Set the new board state.
    ).

move(state(Board, Player, Mode), (Row, Col), state(NewBoard, NextPlayer, Mode), ai) :-
    valid_position(Board, Row, Col),          % Ensure the position is valid.
    nth1(Row, Board, CurrentRow),            % Get the target row.
    nth1(Col, CurrentRow, empty),            % Ensure the target cell is empty.
    replace(Board, Row, Col, Player, TempBoard), % Update the board with the player's piece.
    check_lines(TempBoard, Player, Row, Col, TempBoard2, Mode), % Check for lines of three and handle them.
    (   game_over(state(TempBoard2, Player, Mode), Winner)
    ->  format('Game over! Winner: ~w~n', [Winner]), display_game(state(TempBoard2, Player, Mode)),!, halt
    ;   switch_player(Player, NextPlayer), NewBoard = TempBoard2                   % Set the new board state.
    ).
*/

% Checks for lines of three or more consecutive pieces and handles them.

check_lines(Board, Player, Row, Col, NewBoard, 4) :-
    !,
    (   line_of_three(Board, Player, Row, Col, Line) ->
        handle_line_ai(Board, Player, Line, NewBoard)
    ;   NewBoard = Board
    ).

/*
check_lines(Board, Player, Row, Col, NewBoard, _) :-
    (   line_of_three(Board, Player, Row, Col, Line) ->
        handle_line(Board, Player, Line, NewBoard)
    ;   NewBoard = Board
    ).
*/
check_lines(Board, Player, Row, Col, StackMove) :-
    (   line_of_three(Board, Player, Row, Col, Line) ->
        handle_line(Board, Player, Line, StackMove)
    ;   StackMove = no_stack;
    ).

check_lines(Board, Player, Row, Col, StackMove) :-
    line_of_three(Board, Player, Row, Col, Linw), !,
    handle_line(Board, Player, Line, StackMove).

check_lines(Board, Player, Row, Col, no_stack).



% Handles a line of three or more pieces by asking the user which piece to keep as the stack.
handle_line(Board, Player, Line, NewBoard) :-
    format('Line of three or more found at coordinates: ~w~n', [Line]),
    writeln('Choose which piece to keep as the stack (1, 2, 3, etc): '),
    read(Choice),
    nth1(Choice, Line, (KeepRow, KeepCol)),
    delete(Line, (KeepRow, KeepCol), RemainingLine),
    remove_pieces(RemainingLine, Board, TempBoard),
    replace(TempBoard, KeepRow, KeepCol, stack(Player), NewBoard).

% Handles a line of three or more pieces automatically for AI by keeping the middle piece as the stack.
handle_line_ai(Board, Player, Line, NewBoard) :-
    random(1, 3, Index),
    nth1(Index, Line, (KeepRow, KeepCol)),
    delete(Line, (KeepRow, KeepCol), RemainingLine),
    remove_pieces(RemainingLine, Board, TempBoard),
    replace(TempBoard, KeepRow, KeepCol, stack(Player), NewBoard).

% Removes pieces from the board.
remove_pieces([], Board, Board).
remove_pieces([(Row, Col) | Rest], Board, NewBoard) :-
    replace(Board, Row, Col, empty, TempBoard),
    remove_pieces(Rest, TempBoard, NewBoard).

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

% Checks for a diagonal line of three or more consecutive pieces
diagonal_line(Board, Player, Row, Col, Line) :-
    findall((R, C), (
        adjacent_diagonal_2(Board, Player, Row, Col, R, C)
    ), AdjacentCells),
    length(AdjacentCells, Length),
    Length >= 2,
    are_consecutive_diagonal([(Row, Col)|AdjacentCells]),
    Line = [(Row, Col)|AdjacentCells].

% Modified adjacent_diagonal to ensure consecutiveness
adjacent_diagonal_2(Board, Player, Row, Col, AdjRow, AdjCol) :-
    find_adjacent_diagonal(Board, Player, Row, Col, -1, -1, UpLeftCells),
    find_adjacent_diagonal(Board, Player, Row, Col, -1, 1, UpRightCells),
    find_adjacent_diagonal(Board, Player, Row, Col, 1, -1, DownLeftCells),
    find_adjacent_diagonal(Board, Player, Row, Col, 1, 1, DownRightCells),
    append([UpLeftCells, UpRightCells, DownLeftCells, DownRightCells], AdjacentCells),
    member((AdjRow, AdjCol), AdjacentCells).

% Check if points form a consecutive diagonal line
are_consecutive_diagonal(Points) :-
    sort_points(Points, Sorted),  % Sort points by row
    consecutive_points(Sorted).

% Sort points by row number
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

consecutive_points([]).
consecutive_points([_]).
consecutive_points([(R1, C1), (R2, C2)|Rest]) :-
    abs(R2 - R1) =:= 1,
    abs(C2 - C1) =:= 1,
    consecutive_points([(R2, C2)|Rest]).

% Original find_adjacent_diagonal remains unchanged
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

/*
% Generates a list of all valid moves for the current game state.
valid_moves(state(Board, _, _), Moves) :-
    findall((Row, Col), valid_position(Board, Row, Col), Moves).
*/


valid_moves(state(Board, Player), Moves) :-
    findall(Move , move(state(Board, Player), Move, _), Moves).

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
game_over(state(Board, _), Winner) :-
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

/*
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
*/

choose_move(state(Board, Player), easy_ai, Move) :-
    valid_moves(state(Board, Player), Moves),
    random_member(Move, Moves).

get_gamemode(Gamemode) :-
    writeln('Welcome to LOT! Enter the desired game mode'),
    writeln('1. Human vs Human'),
    writeln('2. Human vs AI'),
    writeln('3. AI vs Human'),
    writeln('4. AI vs AI'),
    read(Number),
    gamemode_number(Number, Gamemode).

gamemode_number(1, h-h).
gamemode_number(2, h-pc).
gamemode_number(3, pc-h).
gamemode_number(4, pc-pc).


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

difficulty_number(1, easy_ai).
difficulty_number(2, medium_ai).
difficulty_number(3, hard_ai).


display_winner((Player, _, _), WinnerColor) :-
    write(Player), write(' won as color '), write(WinnerColor), writeln('!!!').

play :-
    get_gamemode(Gamemode),
    initial_state(7, GameState),
    get_settings(Gamemode, MatchState),
    game_loop(MatchState, GameState, Winner),
    display_winner(MatchState, Winner).

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






/*
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
    play(state(Board, Player, 2), 2, human, AILevel).

play(state(Board, Player, 3)) :-
    writeln('Select the AI level:'),
    writeln('1. Easy'),
    writeln('2. Medium'),
    writeln('3. Hard'),
    read(AILevel),
    play(state(Board, Player, 3), 2, ai, AILevel).

play(state(Board, Player, Mode), 2, human, 1) :-
    display_game(state(Board, Player, Mode)),
    choose_move(state(Board, Player, Mode), human, Move),
    move(state(Board, Player, Mode), Move, NewState),
    play(NewState, 2, ai, 1).

play(state(Board, Player, Mode), 2, ai, 1) :-
    display_game(state(Board, Player, Mode)),
    choose_move(state(Board, Player, Mode), easy_ai, Move),
    move(state(Board, Player, 4), Move, state(Board1, Player1, Mode1)),
    play(state(Board1, Player1, Mode), 2, human, 1).

play(state(Board, Player, Mode), 2, human, 1) :-
    display_game(state(Board, Player, Mode)),
    choose_move(state(Board, Player, Mode), human, Move),
    move(state(Board, Player, Mode), Move, NewState),
    play(NewState, 2, ai, 1).

play(state(Board, Player, Mode), 2, ai, 1) :-
    display_game(state(Board, Player, Mode)),
    choose_move(state(Board, Player, Mode), easy_ai, Move),
    move(state(Board, Player, 4), Move, state(Board1, Player1, Mode1)),
    play(state(Board1, Player1, Mode), 2, human, 1).

play(state(Board, Player, Mode), 2, human, 2) :-
    display_game(state(Board, Player, Mode)),
    choose_move(state(Board, Player, Mode), human, Move),
    move(state(Board, Player, Mode), Move, NewState),
    play(NewState, 2, ai, 2).

play(state(Board, Player, Mode), 2, ai, 2) :-
    display_game(state(Board, Player, Mode)),
    choose_move(state(Board, Player, Mode), medium_ai, Move),
    move(state(Board, Player, 4), Move, state(Board1, Player1, Mode1)),
    display_game(state(Board1, Player1, Mode1)),
    play(state(Board1, Player1, Mode), 2, human, 2).

play(state(Board, Player, 4)) :-
    writeln('Select the AI level for AI1:'),
    writeln('1. Easy'),
    writeln('2. Medium'),
    writeln('3. Hard'),
    read(AILevel1),
    writeln('Select the AI level for AI2:'),
    writeln('1. Easy'),
    writeln('2. Medium'),
    writeln('3. Hard'),
    read(AILevel2),
    play(state(Board, Player, 4), 4, ai1, 1).

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

*/

pie_rulable(Board) :- one_piece(Board).

one_piece([Head|Tail]) :-      % applies for both Row-Board and Element-Row pairs
    one_piece(Head),
    no_piece(Tail).
one_piece([Head|Tail]) :-
    no_piece(Head),
    one_piece(Tail).

one_piece(white).
one_piece(black).

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
value(Board, Player, Value) :-
    count_lines_of_two(Board, Player, LineOfTwoSingleCount),
    count_stacks(Board, Player, StackCount),
    count_lines_of_two(Board, stack(Player), LineOfTwoStackCount),
    Value is LineOfTwoSingleCount + StackCount*4 + LineOfTwoStackCount*2.

% counts lines of 2 uninterrupted (by opposite color) pieces (stack or single) of Player color
count_lines_of_two(Board, Player, Count) :-
    findall(1, (nth1(Row, Board, CurrentRow),  nth1(Col, CurrentRow, empty), line_of_three(Board, Player, CurrentRow, Col, _)), FavorableCells),
    length(FavorableCells, Count).

count_stacks(Board, Player, Count) :-
    findall(1, (nth1(Row, Board, CurrentRow),  nth1(Col, CurrentRow, stack(Player))), Stacks),
    length(Stacks, Count).

% medium -> best value achievable in 1 move
choose_move(state(Board, Player), medium_ai, BestMove) :-
    valid_moves(state(Board, Player), Moves),
    setof((Value, Move), NewState^(move(state(Board, Player), Move, NewState), value(NewState, Player, Value)), ValueMoveMap),
    last(ValueMoveMap, (_, BestMove)).  % last has the highest value since setof sorts elements

% hard -> best value achievable in 2 moves, while trying to predict opponent's next move (minimax)
choose_move(state(Board, Player), hard_ai, BestMove) :-
    valid_moves(state(Board, Player), MyMoves),
    switch_player(Player, Opponent),
    findall((MyMove, OpMove), NewState^(move(state(Board,Player), MyMove, NewState), choose_move(NewState, medium_ai, OpMove)), OpPredictions).


ask_place(Position) :-
    writeln('Enter your move row:'),
    read(Row),
    writeln('Enter your move column:'),
    read(Col),
    Position = (Row, Col).

ask_pie_rule(PieRule) :-
    writeln('Would you like to apply the pie rule? (y/n)'),
    read(Answer),
    pie_rule_answer(Answer, PieRule).

pie_rule_answer(y, pie_rule).
pie_rule_answer(n, no_pie_rule).


ask_build_stack(StackMove) :-
    writeln('Would you like to build a stack? (y/n)'),
    read(Answer),
    ask_stack_move(Answer, StackMove).

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



choose_move(state(Board, Player), human, Move) :-
    ask_pie_rule(PieRule),
    ask_place(Position),
    ask_build_stack(StackMove),
    Move = (Position, StackMove, PieRule).


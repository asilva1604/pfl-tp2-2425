:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(random)).
:- consult('game_states.pl').
:- consult('utils.pl').
:- consult('lines.pl').
:- consult('io.pl').


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

% display_winner(+MatchState, +Winner)
% Displays the winner of the game.
% MatchState: The current state of the match.
% Winner: The winner of the game.

display_winner((Player, _, _), draw) :-
    writeln('Draw! Everybody wins!!! ^_^').

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
    repeat,
    display_game((player1, _, _), GameState),
    \+ game_over(GameState, Winner),
    choose_move(GameState, PType1, Move),
    move(GameState, Move, NewGameState),
    game_loop((player2, PType1, PType2), NewGameState, Winner).

game_loop((player2, PType1, PType2), GameState, Winner) :- 
    repeat,
    display_game((player2, _, _), GameState),
    \+ game_over(GameState, Winner),
    choose_move(GameState, PType2, Move),
    move(GameState, Move, NewGameState),
    game_loop((player1, PType1, PType2), NewGameState, Winner).

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

% choose_move(+GameState, +Level, -Move)
% Chooses a move for the given level of AI, or for a human.
% GameState: The current state of the game.
% Level: The level of the AI (easy_ai, medium_ai, hard_ai), or human.
% Move: The chosen move.
choose_move(state(Board, Player), easy_ai, Move) :-
    valid_moves(state(Board, Player), Moves),
    random_member(Move, Moves).


% medium -> best value achievable in 1 move

choose_move(state(Board, Player), medium_ai, ChosenMove) :-
    valid_moves(state(Board, Player), _),
    setof((Value, Move), NewState^(move(state(Board, Player), Move, NewState), value(NewState, Player, Value)), ValueMoveMap),
    last(ValueMoveMap, (BestValue, _)),  % last has the highest value since setof sorts elements
    findall(BestMove, member((BestValue, BestMove), ValueMoveMap), BestMoves),
    random_member(ChosenMove, BestMoves).

% hard -> best value achievable in 2 moves, while trying to predict opponent's next move (minimax)
choose_move(state(Board, Player), hard_ai, ChosenMove) :-
    valid_moves(state(Board, Player), MyMoves),
    switch_player(Player, Opponent),
    findall((MyMove, OpMove), NewState^(move(state(Board,Player), MyMove, NewState), choose_move(NewState, medium_ai, OpMove)), OpPredictions),
    setof((Value, MyMove1), (member((MyMove1, OpMove1), OpPredictions), value_diff(Board, Player, MyMove1, OpMove1, Value)), ValueMoveMap),

    last(ValueMoveMap, (BestValue, _)),  % last has the highest value since setof sorts elements
    findall(BestMove, member((BestValue, BestMove), ValueMoveMap), BestMoves),
    random_member(ChosenMove, BestMoves).


value_diff(Board, PlayerColor, MyMove, OpMove, Value) :-
    move(state(Board, PlayerColor), MyMove, BoardAfterMe),
    value(BoardAfterMe, PlayerColor, MyValue),

    switch_player(PlayerColor, OpColor),
    move(state(BoardAfterMe, OpColor), OpMove, OpValue),

    Value is MyValue - OpValue.


choose_move(state(_, _), human, Move) :-
    ask_pie_rule(PieRule),
    ask_place(Position),
    ask_build_stack(StackMove),
    Move = (Position, StackMove, PieRule).

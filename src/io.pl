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

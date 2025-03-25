/* 
    Автор: Васильєв Михайло Сергійович 
    Завдання: Реалізація логіки гри "Хрестики-Нулики" з ботом із чотирма рівнями складності.
    Приклади запитів наведені після визначень предикатів.

    Коментарі до використаних вбудованих предикатів та функторів:
    nth0/3 – стандартний предикат для отримання елемента списку за індексом (нумерація з нуля).

    length/2 – визначає довжину списку.

    between/3 – генерує числа в заданому діапазоні (вхідні параметри мають бути конкретизованими).

    findall/3 – збирає всі рішення заданого запиту в список.

    atom_concat/3 – об'єднує два атоми.

    atom_chars/2 – перетворює атом у список символів (і навпаки).

    random_member/2 – вибирає випадковий елемент зі списку.

    sort/3 – сортує список, усуваючи дублікати.
*/

/* ===================== HELPER PREDICATES ===================== */

/** cell_at(++Board, ++R, ++C, --Value)
    Отримує значення комірки (Value) з двовимірного списку Board за індексами R і C.
    
    Приклади:
    ?- cell_at([[a,b],[c,d]], 0, 1, V).
       V = b.
    ?- cell_at([[a,b],[c,d]], 1, 0, V).
       V = c.
*/
cell_at(Board, R, C, Value) :-
    nth0(R, Board, Row),
    nth0(C, Row, Value).

/** board_dimensions(++Board, --Rows, --Cols)
    Визначає кількість рядків (Rows) і стовпців (Cols) у Board.
    
    Приклад:
    ?- board_dimensions([[1,2,3],[4,5,6]], Rows, Cols).
       Rows = 2,
       Cols = 3.
*/
board_dimensions(Board, Rows, Cols) :-
    length(Board, Rows),
    Board = [FirstRow|_],
    length(FirstRow, Cols).

/** direction(++DR, ++DC)
    Факти, що задають напрямки (горизонтально, вертикально, діагонально).
    
    Приклади:
    ?- direction(DR, DC).
       DR = 0, DC = 1 ;
       DR = 1, DC = 0 ;
       DR = 1, DC = 1 ;
       DR = -1, DC = 1.
*/
direction(0, 1).
direction(1, 0).
direction(1, 1).
direction(-1, 1).

/* ===================== WIN CHECKING ===================== */

/** line_in_direction(++Board, ++R, ++C, ++DR, ++DC, ++N, ++Player, --Positions)
    Визначає позиції Positions (список пар (Row,Col)), починаючи з (R,C) у напрямку (DR,DC)
    довжиною N, якщо всі комірки рівні Player.
    
    Приклад:
    ?- line_in_direction([[x,x,x],[o,empty,empty],[empty,empty,empty]], 0, 0, 0, 1, 3, x, Pos).
       Pos = [(0,0),(0,1),(0,2)].
*/
line_in_direction(Board, R, C, DR, DC, N, Player, Positions) :-
    N1 is N - 1,
    findall((RR, CC),
        ( between(0, N1, I),
          RR is R + I * DR,
          CC is C + I * DC,
          cell_at(Board, RR, CC, Val),
          Val == Player
        ),
        Positions
    ),
    length(Positions, N).

/** check_winner(++Board, ++NToWin, --Winner, --WinLine)
    Знаходить переможця у Board, якщо існує послідовність з NToWin однакових елементів.
    Якщо Board повністю заповнений і немає переможця, Winner = "draw".
    
    Приклад:
    ?- check_winner([[x,x,x],[o,empty,empty],[empty,empty,empty]], 3, W, WL).
       W = "X",
       WL = [(0,0),(0,1),(0,2)].
*/
check_winner(Board, N, Winner, WinLine) :-
    board_dimensions(Board, Rows, Cols),
    RMax is Rows - 1,
    CMax is Cols - 1,
    between(0, RMax, R),
    between(0, CMax, C),
    cell_at(Board, R, C, Cell),
    Cell \= empty,
    direction(DR, DC),
    line_in_direction(Board, R, C, DR, DC, N, Cell, WinLine),
    ( Cell = x -> Winner = "X" ; Cell = o -> Winner = "O" ),
    !.
check_winner(Board, _N, "draw", []) :-
    \+ ( member(Row, Board), member(empty, Row) ).

/* ===================== BOARD CELL COLLECTION ===================== */

/** get_empty_cells(++Board, --EmptyCells)
    Знаходить усі порожні комірки Board. Результат – список пар (Row,Col).
    
    Приклад:
    ?- get_empty_cells([[x,empty],[o,empty]], EC).
       EC = [(0,1),(1,1)].
*/
get_empty_cells(Board, EmptyCells) :-
    findall((R, C),
        ( nth0(R, Board, Row),
          nth0(C, Row, Cell),
          Cell == empty
        ),
        EmptyCells
    ).

/** valid_index(++Board, ++R, ++C)
    Перевіряє, чи індекси (R, C) знаходяться в межах Board.
    
    Приклад:
    ?- valid_index([[a,b],[c,d]], 1, 1).
       true.
    ?- valid_index([[a,b],[c,d]], 2, 0).
       false.
*/
valid_index(Board, R, C) :-
    board_dimensions(Board, Rows, Cols),
    R >= 0, R < Rows,
    C >= 0, C < Cols.

/** adjacent_non_empty(++Board, ++R, ++C, ++Radius)
    Перевіряє, чи є принаймні одна непорожня комірка в радіусі Radius від (R, C).
    
    Приклад:
    ?- adjacent_non_empty([[empty,empty],[x,empty]], 0, 0, 1).
       true.
*/
adjacent_non_empty(Board, R, C, Radius) :-
    Min is -Radius, Max is Radius,
    between(Min, Max, DR),
    between(Min, Max, DC),
    (DR \= 0 ; DC \= 0),
    R2 is R + DR,
    C2 is C + DC,
    valid_index(Board, R2, C2),
    cell_at(Board, R2, C2, Val),
    Val \= empty,
    !.

/** get_active_zone(++Board, ++Radius, --ActiveCells)
    Знаходить усі порожні комірки, які знаходяться поруч із заповненими (в радіусі Radius).
    
    Приклад:
    ?- get_active_zone([[empty,empty],[x,empty]], 1, AZ).
       AZ = [(0,0),(0,1),(1,1)].
*/
get_active_zone(Board, Radius, ActiveCells) :-
    findall((R, C),
        ( nth0(R, Board, Row),
          nth0(C, Row, Cell),
          Cell == empty,
          adjacent_non_empty(Board, R, C, Radius)
        ),
        Cells
    ),
    sort(Cells, ActiveCells).

/* ===================== LINE GENERATION FOR PATTERN DETECTION ===================== */

/** get_line(++Board, ++R, ++C, ++DR, ++DC, --Line)
    Створює атом, що представляє лінію з 11 символів (від -5 до 5) у напрямку (DR, DC),
    починаючи з (R, C). Символи: '#' (поза межами), '.' (порожньо), 'x' або 'o' (зайнято).
    
    Приклад:
    ?- get_line([[x,x,x],[empty,empty,empty],[empty,empty,empty]], 0, 0, 0, 1, Line).
       Line = "xxx#######" (наприклад, залежно від положення).
*/
get_line(Board, R, C, DR, DC, Line) :-
    MinI is -5, MaxI is 5,
    findall(Char,
        ( between(MinI, MaxI, I),
          R2 is R + I * DR,
          C2 is C + I * DC,
          ( \+ valid_index(Board, R2, C2) ->
              Char = '#'
            ; cell_at(Board, R2, C2, Val),
              ( Val == empty -> Char = '.'
              ; Val = x      -> Char = 'x'
              ; Val = o      -> Char = 'o'
              )
            )
        ),
        Chars
    ),
    atom_chars(Line, Chars).

/** get_lines(++Board, ++R, ++C, --Lines)
    Отримує список ліній (по 4 напрямках) для комірки (R, C).
    
    Приклад:
    ?- get_lines([[x,empty],[empty,empty]], 0, 0, L).
       L = ["x.", "#.", ...].  % Чотири напрямки.
*/
get_lines(Board, R, C, Lines) :-
    findall(Line,
        ( member((DR, DC), [(1,0), (0,1), (1,1), (-1,1)]),
          get_line(Board, R, C, DR, DC, Line)
        ),
        Lines
    ).

/* ===================== PATTERN DETECTION ===================== */

/** replicate(++Char, ++Count, --Atom)
    Повторює символ Char Count разів і повертає результат в Atom.
    
    Приклад:
    ?- replicate(x, 4, A).
       A = "xxxx".
*/
replicate(_, 0, "") :- !.
replicate(Char, N, Atom) :-
    N > 0,
    N1 is N - 1,
    replicate(Char, N1, Rest),
    atom_concat(Char, Rest, Atom).

/** patterns(++Player, ++Opponent, --Patterns)
    Визначає список патернів для заданих Player та Opponent разом з оцінками.
    
    Приклад:
    ?- patterns(x, o, P).
       P = [("xxxx.",10000), (".xxxx",10000), ("xxx.x",8000), ...].
*/
patterns(Player, Opponent, Patterns) :-
    replicate(Player, 4, P4),
    replicate(Player, 3, P3),
    replicate(Player, 2, P2),
    replicate(Opponent, 4, O4),
    atom_concat(P4, ".", Pat1),
    atom_concat(".", P4, Pat2),
    atom_concat(P3, ".", Temp1),
    atom_concat(Temp1, Player, Pat3),
    atom_concat(Player, P3, Pat4),
    atom_concat(P2, ".", Temp2),
    atom_concat(Temp2, P2, Pat5),
    atom_concat(".", O4, Pat6),
    Patterns = [
        (Pat1, 10000),
        (Pat2, 10000),
        (Pat3, 8000),
        (Pat4, 8000),
        (Pat5, 9000),
        (O4,   9500),
        (Pat6, 9500)
    ].

/** detect_patterns(++Board, ++R, ++C, ++Player, ++Opponent, ?_N, --Score)
    Визначає максимальну оцінку Score, яку можна отримати за паттернами для комірки (R,C).
    _N не використовується.
    
    Приклад:
    ?- detect_patterns([[x,x,x,empty],[empty,empty,empty,empty],[empty,empty,empty,empty],[empty,empty,empty,empty]], 0, 3, x, o, _N, Score).
       Score = 8000.  % Наприклад, для "xxx.x"
*/
detect_patterns(Board, R, C, Player, Opponent, _N, Score) :-
    get_lines(Board, R, C, Lines),
    patterns(Player, Opponent, PatternList),
    findall(Val,
        ( member(Line, Lines),
          member((Pat, Val), PatternList),
          sub_atom(Line, _, _, _, Pat)
        ),
        Scores
    ),
    ( Scores = [] -> Score = 0 ; max_list(Scores, Score) ).

/* ===================== POSITION EVALUATION ===================== */

/** evaluate_position(++Board, ++R, ++C, ++Player, ++NToWin, --Score)
    Оцінює позицію (R,C) для Player за допомогою максимального значення з усіх напрямків.
    
    Приклад:
    ?- evaluate_position([[x,empty],[empty,empty]], 0, 0, x, 2, Score).
       Score = 10.  % Наприклад.
*/
evaluate_position(Board, R, C, Player, N, Score) :-
    findall(S,
        ( member((DR, DC), [(1,0), (0,1), (1,1), (-1,1)]),
          evaluate_direction(Board, R, C, DR, DC, Player, N, S)
        ),
        Scores
    ),
    ( Scores = [] -> Score = 0 ; max_list(Scores, Score) ).

/** evaluate_direction(++Board, ++R, ++C, ++DR, ++DC, ++Player, ++NToWin, --Score)
    Оцінює напрямок (DR,DC) від (R,C) для Player, повертаючи Score як 10^(кількість послідовних фішок), якщо не заблоковано.
    
    Приклад:
    ?- evaluate_direction([[x,x,empty],[empty,empty,empty],[empty,empty,empty]], 0, 0, 0, 1, x, 3, Score).
       Score = 100.  % Якщо 2 послідовні "x" без блокування.
*/
evaluate_direction(Board, R, C, DR, DC, Player, N, Score) :-
    count_in_direction(Board, R, C, DR, DC, Player, Count1, Block1),
    count_in_direction(Board, R, C, -DR, -DC, Player, Count2, Block2),
    Total is Count1 + Count2 - 1,
    Blocks is Block1 + Block2,
    ( Blocks < 2 -> Score is 10 ^ Total ; Score = 0 ).

/** count_in_direction(++Board, ++R, ++C, ++DR, ++DC, ++Player, --Count, --Blocks)
    Рахує кількість послідовних елементів Player в напрямку (DR,DC) від (R,C).
    
    Приклад:
    ?- count_in_direction([[x,x,empty],[empty,empty,empty],[empty,empty,empty]], 0, 0, 0, 1, x, Count, Blocks).
       Count = 2, Blocks = 0.
*/
count_in_direction(Board, R, C, DR, DC, Player, Count, Blocks) :-
    next_count(Board, R, C, DR, DC, Player, 0, Count, 0, Blocks).

/** next_count(++Board, ++R, ++C, ++DR, ++DC, ++Player, ++Acc, --Count, ++BlockAcc, --Blocks)
    Рекурсивно підраховує Count та Blocks у напрямку (DR,DC).
    
    Приклад:
    ?- next_count([[x,x,empty],[empty,empty,empty],[empty,empty,empty]], 0, 0, 0, 1, x, 0, Count, 0, Blocks).
       Count = 2, Blocks = 0.
*/
next_count(Board, R, C, DR, DC, Player, Acc, Count, BlockAcc, Blocks) :-
    R2 is R + DR,
    C2 is C + DC,
    ( valid_index(Board, R2, C2) ->
        cell_at(Board, R2, C2, Val),
        ( Val == Player ->
            Acc2 is Acc + 1,
            next_count(Board, R2, C2, DR, DC, Player, Acc2, Count, BlockAcc, Blocks)
        ; Val \= empty ->
            Blocks is BlockAcc + 1,
            Count is Acc + 1
        ; Count is Acc + 1,
          Blocks = BlockAcc
        )
    ; Count is Acc + 1,
      Blocks is BlockAcc + 1
    ).

/* ===================== CRITICAL THREAT DETECTION ===================== */

/** detect_critical_threat(++Board, ++Player, ++NToWin, --Move)
    Якщо розміщення фішки Player у порожній клітинці миттєво приводить до перемоги,
    повертає цю позицію як Move.
    
    Приклад:
    ?- detect_critical_threat([[x,x,empty],[o,empty,empty],[empty,empty,empty]], x, 3, Move).
       Move = (0,2).
*/
detect_critical_threat(Board, Player, N, Move) :-
    get_empty_cells(Board, EmptyCells),
    member(Move, EmptyCells),
    set_cell(Board, Move, Player, NewBoard),
    check_winner(NewBoard, N, Winner, _),
    ( Player == x -> Winner == "X" ; Winner == "O" ),
    !.

/* ===================== BOARD UPDATE ===================== */

/** set_cell(++Board, ++(R,C), ++Player, --NewBoard)
    Повертає NewBoard, який є Board з елементом Player, розміщеним у позиції (R,C).
    
    Приклад:
    ?- set_cell([[x,empty],[empty,empty]], (0,1), x, NB).
       NB = [[x,x],[empty,empty]].
*/
set_cell(Board, (R, C), Player, NewBoard) :-
    nth0(R, Board, OldRow),
    replace_nth(OldRow, C, Player, NewRow),
    replace_nth(Board, R, NewRow, NewBoard).

/** replace_nth(++List, ++Index, ++Elem, --NewList)
    Замінює елемент у List на позиції Index на Elem, повертаючи NewList.
    
    Приклад:
    ?- replace_nth([a,b,c], 1, x, NL).
       NL = [a,x,c].
*/
replace_nth([_|T], 0, Elem, [Elem|T]) :- !.
replace_nth([H|T], I, Elem, [H|R]) :-
    I > 0,
    I2 is I - 1,
    replace_nth(T, I2, Elem, R).

/* ===================== BOT MOVE SELECTION ===================== */

/** opponent(++Turn, --Opponent)
    Визначає супротивника для даного Turn.
    
    Приклади:
    ?- opponent(x, O).
       O = o.
    ?- opponent(o, O).
       O = x.
*/
opponent(x, o).
opponent(o, x).

/** fix_move(?M, --FixedMove)
    Гарантує, що M представлено як список [R, C]. Якщо M уже список – повертає його,
    інакше, якщо воно є термом у вигляді ',(R,C)', перетворює у список.
    
    Приклади:
    ?- fix_move((3,5), FM).
       FM = [3,5].
    ?- fix_move([2,4], FM).
       FM = [2,4].
*/
fix_move(M, M) :- is_list(M), !.
fix_move(M, [R, C]) :-
    M =.. [',', R, C].

/** bot_move(++Board, ++Turn, ++Difficulty, ++NToWin, --FixedMove)
    Визначає хід для бота на основі заданого рівня складності.
    Рівні:
      - "very easy": рандомний хід.
      - "easy": спочатку спроба знайти критичну загрозу (свою або супротивника), інакше рандом.
      - "medium": 10% шанс рандомного ходу, інакше критична загроза, інакше best_move.
      - "hard": 2% шанс рандомного ходу, інакше критична загроза, інакше best_move_hard.
    FixedMove – список [R,C].
    
    Приклад:
    ?- bot_move([[empty,empty,empty],[empty,x,empty],[empty,empty,empty]], o, "very easy", 3, Move).
       Move = [0,2].  % Наприклад.
*/
bot_move(Board, Turn, Difficulty0, N, FixedMove) :-
    ( string(Difficulty0) -> atom_string(Difficulty, Difficulty0)
    ; Difficulty = Difficulty0
    ),
    ( Difficulty = 'very easy' ->
         random_move(Board, Move)
    ; Difficulty = 'easy' ->
         ( detect_critical_threat(Board, Turn, N, Move) -> true
         ; opponent(Turn, Opp),
           detect_critical_threat(Board, Opp, N, Move) -> true
         ; random_move(Board, Move)
         )
    ; Difficulty = 'medium' ->
         random(RandomVal),
         ( RandomVal < 0.10 ->
             random_move(Board, Move)
         ; ( detect_critical_threat(Board, Turn, N, Move) -> true
           ; opponent(Turn, Opp),
             detect_critical_threat(Board, Opp, N, Move) -> true
           ; best_move(Board, Turn, N, Move)
           )
         )
    ; Difficulty = 'hard' ->
         random(RandomVal2),
         ( RandomVal2 < 0.02 ->
             random_move(Board, Move)
         ; ( detect_critical_threat(Board, Turn, N, Move) -> true
           ; opponent(Turn, Opp),
             detect_critical_threat(Board, Opp, N, Move) -> true
           ; best_move_hard(Board, Turn, N, Move)
           )
         )
    ;  % fallback if Difficulty is unrecognized
       random_move(Board, Move)
    ),
    fix_move(Move, FixedMove).

/** random_move(++Board, --Move)
    Вибирає рандомну позицію з активної зони (radius = 4), або, якщо вона порожня,
    серед усіх порожніх комірок.
    
    Приклад:
    ?- random_move([[empty,x],[empty,empty]], M).
       M = (0,0) ; M = (1,0) ; M = (1,1).
*/
random_move(Board, Move) :-
    get_active_zone(Board, 4, Active),
    ( Active \= [] -> List = Active ; get_empty_cells(Board, List) ),
    random_member(Move, List).

/** best_move(++Board, ++Turn, ++NToWin, --BestMove)
    Для "medium" рівня: вибирає позицію, що максимізує (2*self_score + opp_score).
    
    Приклад:
    ?- best_move([[empty,x,empty],[empty,empty,empty],[empty,empty,empty]], x, 3, BM).
       BM = (0,2).  % Наприклад.
*/
best_move(Board, Turn, N, BestMove) :-
    opponent(Turn, Opp),
    get_empty_cells(Board, Empty),
    findall((Score, Pos),
        ( member(Pos, Empty),
          Pos = (R, C),
          evaluate_position(Board, R, C, Turn, N, SelfScore),
          evaluate_position(Board, R, C, Opp,   N, OppScore),
          Score is SelfScore * 2 + OppScore
        ),
        ScoredMoves
    ),
    sort(0, @>=, ScoredMoves, Sorted),
    Sorted = [(_, BestMove)|_].

/** best_move_hard(++Board, ++Turn, ++NToWin, --BestMove)
    Для "hard" рівня: використовує оцінку патернів * 100 плюс позиційну оцінку.
    
    Приклад:
    ?- best_move_hard([[empty,x,empty],[empty,empty,empty],[empty,empty,empty]], x, 3, BM).
       BM = (0,2).  % Наприклад.
*/
best_move_hard(Board, Turn, N, BestMove) :-
    opponent(Turn, Opp),
    get_empty_cells(Board, Empty),
    findall((Score, Pos),
        ( member(Pos, Empty),
          Pos = (R, C),
          detect_patterns(Board, R, C, Turn, Opp, N, PatternScore),
          evaluate_position(Board, R, C, Turn, N, HeuristicScore),
          Score is PatternScore * 100 + HeuristicScore
        ),
        ScoredMoves
    ),
    sort(0, @>=, ScoredMoves, Sorted),
    Sorted = [(_, BestMove)|_].

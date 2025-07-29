:- use_module(library(clpfd)).
:- use_module(library(random)).

:- discontiguous insert_value/5.
:- discontiguous print_grid/1.
:- discontiguous print_rows_ascii/2.
:- discontiguous print_row_ascii/1.
:- discontiguous cell/1.

%% =============================================================================
%% SELEZIONE DIFFICOLTA
%% =============================================================================

select_difficulty(Difficulty) :-
    nl,
    write('=== SELEZIONE DIFFICOLTA ==='), nl,
    write('1. Facile'), nl,
    write('2. Medio'),  nl,
    write('3. Difficile'), nl,
    write('Scelta: '),
    catch(read(Scelta), _, select_difficulty(Difficulty)),
    difficulty_choice(Scelta, Difficulty).

difficulty_choice(1, facile).
difficulty_choice(2, medio).
difficulty_choice(3, difficile).
difficulty_choice(_, D) :-
    write('Scelta non valida. Riprova.'), nl,
    select_difficulty(D).

difficulty_cells(facile, 35).
difficulty_cells(medio, 50).
difficulty_cells(difficile, 60).

%% =============================================================================
%% play/0: avvia il gioco
%% =============================================================================
play :-
    write('Benvenuto al Sudoku!'), nl,
    select_difficulty(Difficulty),
    format('Generazione griglia in corso... (Difficolta: ~w)~n', [Difficulty]),
    difficulty_cells(Difficulty, NumRemove),
    generate_grid(NumRemove, Puzzle),
    write('Ecco il puzzle:'), nl,
    print_grid(Puzzle),
    game_loop(Puzzle, 0).

%% =============================================================================
%% game_loop/2: menu interattivo con contatore tentativi
%% =============================================================================
game_loop(Grid, Attempts) :-
    nl,
    (Attempts >= 3 ->
        write('Hai esaurito i tentativi! Game Over.'), nl, !
    ;
        (completed(Grid) ->
            write('Complimenti, Sudoku completato!'), nl
        ; true),
        Remaining is 3 - Attempts,
        format('Tentativi rimanenti: ~w~n', [Remaining]),
        write('=== MENU SUDOKU ==='), nl,
        write('1. Inserisci numero'), nl,
        write('2. Mostra griglia'),  nl,
        write('3. Esci'),             nl,
        write('Scelta: '),
        catch(read(Scelta), _, (write('Input non valido!'), nl, game_loop(Grid, Attempts))),
        process_choice(Scelta, Grid, Attempts)
    ).

%% =============================================================================
%% process_choice/3: gestisce le scelte del menu con contatore tentativi
%% =============================================================================
process_choice(1, Grid, Attempts) :-
    write('Riga (1-9): '),    catch(read(R), _, invalid_input(Grid, Attempts)),
    write('Colonna (1-9): '), catch(read(C), _, invalid_input(Grid, Attempts)),
    ( \+ between(1,9,R) ; \+ between(1,9,C) ->
        write('Valori fuori dal range!'), nl,
        game_loop(Grid, Attempts)
    ;
        nth1(R, Grid, Row), nth1(C, Row, Existing),
        ( Existing \= 0 ->
            write('È già presente un numero in quella posizione!'), nl,
            game_loop(Grid, Attempts)
        ;
            write('Valore (1-9): '), catch(read(V), _, invalid_input(Grid, Attempts)),
            ( \+ between(1,9,V) -> 
                write('Valore fuori dal range!'), nl,
                game_loop(Grid, Attempts)
            ;
                ( valid_move(Grid,R,C,V) ->
                    insert_value(Grid,R,C,V,NewGrid),
                    write('Inserimento completato.'), nl,
                    print_grid(NewGrid),
                    game_loop(NewGrid, Attempts)
                ;
                    write('Mossa non valida!'), nl,
                    NewAttempts is Attempts + 1,
                    game_loop(Grid, NewAttempts)
                )
            )
        )
    ).

process_choice(2, Grid, Attempts) :-
    print_grid(Grid),
    game_loop(Grid, Attempts).

process_choice(3, _, _) :-
    write('Grazie e a presto!'), nl,
    !.

process_choice(_, Grid, Attempts) :-
    write('Scelta non valida!'), nl,
    game_loop(Grid, Attempts).

invalid_input(Grid, Attempts) :-
    write('Input non valido!'), nl,
    game_loop(Grid, Attempts).

%% =============================================================================
%% valid_move/4: controllo dei vincoli per (R,C)=V
%% =============================================================================
valid_move(Grid,R,C,V) :-
    between(1,9,R), between(1,9,C), between(1,9,V),
    nth1(R,Grid,Row), nth1(C,Row,0),
    \+ member(V,Row),
    transpose(Grid,Cols), nth1(C,Cols,Col),
    \+ member(V,Col),
    block_coordinates(R,C,BR,BC),
    get_block(Grid,BR,BC,Block),
    \+ member(V,Block).

block_coordinates(R,C,BR,BC) :-
    BR is ((R-1)//3)+1,
    BC is ((C-1)//3)+1.

get_block(Grid,BR,BC,Block) :-
    SR is (BR-1)*3+1,
    SC is (BC-1)*3+1,
    findall(X,
        ( between(0,2,DR), between(0,2,DC),
          R is SR+DR, C is SC+DC,
          nth1(R,Grid,Row), nth1(C,Row,X)
        ),
        Block).

%% =============================================================================
%% insert_value/5: costruisce NewGrid con V in (R,C)
%% =============================================================================
insert_value(Grid,R,C,V,NewGrid) :-
    nth1(R, Grid, Row),
    replace_nth(C, Row, V, NewRow),
    replace_nth(R, Grid, NewRow, NewGrid).

replace_nth(1, [_|T], X, [X|T]).
replace_nth(N, [H|T], X, [H|R]) :-
    N>1, N1 is N-1,
    replace_nth(N1, T, X, R).

completed(Grid) :-
    \+ (member(Row, Grid), member(0, Row)).

%% =============================================================================
%% Generazione randomica del puzzle con livello di difficoltà
%% =============================================================================
generate_grid(NumRemove, Puzzle) :-
    length(Rows,9), maplist(same_length(Rows), Rows),
    append(Rows,Vs), Vs ins 1..9,
    maplist(all_distinct, Rows),
    transpose(Rows,Cols), maplist(all_distinct,Cols),
    Rows=[R1,R2,R3,R4,R5,R6,R7,R8,R9],
    blocks(R1,R2,R3), blocks(R4,R5,R6), blocks(R7,R8,R9),
    label(Vs),
    findall((R,C,Rs,Cs),
        ( between(1,9,R), between(1,9,C),
          Rs is 10-R, Cs is 10-C,
          ( R<Rs ; R=Rs, C=<Cs )
        ),
        Pairs),
    random_permutation(Pairs, Shuffled),
    Needed is ceiling(NumRemove/2),
    length(Selected,Needed),
    append(Selected,_,Shuffled),
    remove_pairs(Selected, Rows, Puzzle).

remove_pairs([], Grid, Grid).
remove_pairs([(R,C,Rs,Cs)|T], G0, Gout) :-
    remove_cell(G0,R,C,G1),
    remove_cell(G1,Rs,Cs,G2),
    remove_pairs(T, G2, Gout).

remove_cell(Grid,R,C,NewGrid) :-
    nth1(R,Grid,Row), nth1(C,Row,V), V\=0, !,
    replace_nth(C,Row,0,NewRow),
    replace_nth(R,Grid,NewRow,NewGrid).
remove_cell(Grid,_,_,Grid).

blocks([],[],[]).
blocks([A,B,C|R1],[D,E,F|R2],[G,H,I|R3]) :-
    all_distinct([A,B,C,D,E,F,G,H,I]),
    blocks(R1,R2,R3).

%% =============================================================================
%% Stampa ASCII della griglia
%% =============================================================================
print_grid(Grid) :-
    write('    1 2 3   4 5 6   7 8 9'), nl,
    print_separator,
    print_rows_ascii(Grid,1).

print_separator :-
    write('  +-------+-------+-------+'), nl.

print_rows_ascii([],_) :-
    print_separator.
print_rows_ascii([R|Rs],N) :-
    format('~w | ',[N]),
    print_row_ascii(R), nl,
    ( N mod 3 =:= 0 ->
        print_separator
    ;  write('  |       |       |       |'), nl
    ),
    N1 is N+1,
    print_rows_ascii(Rs,N1).

print_row_ascii([A,B,C,D,E,F,G,H,I]) :-
    cell(A), cell(B), cell(C), write('| '),
    cell(D), cell(E), cell(F), write('| '),
    cell(G), cell(H), cell(I), write('|').

cell(0) :- write('. ').
cell(X) :- write(X), write(' ').

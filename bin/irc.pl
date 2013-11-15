#!/run/current-system/sw/bin/swipl -q -t main -f

:- use_module(library(ansi_term)).
:- use_module(ii_log_dcg, [line//4]).

:- prompt(_, '').

:- on_signal(int, _, handle_signal).

handle_signal(int) :-
    halt.

main :-
    forall((repeat, 
            read_line_to_codes(user_input, Text)
           ),
           (phrase(line(Date, Time, User_Id, Message), Text),
            user_id_color(User_Id, Color),
            format('~w ~w', [Date, Time]),
            ansi_format([fg(Color), bold], ' <~w> ', [User_Id]),
            format('~w~n', [Message])
           )
          ).

colors([black, red, green, yellow, blue, magenta, cyan]).

user_id_color('-!-', white).
user_id_color(User_Id, Color) :-
    User_Id \== '-!-',
    atom_codes(User_Id, User_Id_Codes),
    sum_list(User_Id_Codes, Integer),
    colors(Colors),
    length(Colors, Number_Of_Colors),
    Index is powm(Integer, 1, Number_Of_Colors),
    nth0(Index, Colors, Color).

:- module(ii_log_dcg, [ii_log//1, line//4]).

:- use_module(library(dcg/basics)).

ii_log([]) -->
    [].
ii_log([line(Date, Time, User_Id, Message)|Lines]) -->
    line(Date, Time, User_Id, Message),
    "\n",
    ii_log(Lines).

line(Date, Time, User_Id, Message) -->
    date(Date),
    " ",
    time(Time),
    " ",
    user_id(User_Id),
    " ",
    string_without("\n", Text),
    {
        string_to_atom(Text, Message)
    }.

date(Date) -->
    digits(Year_Codes),%digiteger(Year),
    "-",
    digits(Month_Codes), %digiteger(Month),
    "-",
    digits(Day_Codes), %digiteger(Day),
    { 
        string_to_atom(Year_Codes, Year),
        string_to_atom(Month_Codes, Month),
        string_to_atom(Day_Codes, Day),
        atomic_list_concat([Year, Month , Day], '-', Date)
    }.

time(Time) -->
    digits(Hour_Codes), %digiteger(Hour),
    ":",
    digits(Minute_Codes), %digiteger(Minute),
    {
        string_to_atom(Hour_Codes, Hour),
        string_to_atom(Minute_Codes, Minute),
        atomic_list_concat([Hour, Minute], ':', Time)
    }.

user_id(Nickname) -->
    "<",
    ascii(Nick),
    ">",
    {
        string_to_atom(Nick, Nickname)
    }.
user_id('-!-') -->
    "-!-".

% probleempje, gebruikte alnum maar nick mag meer dan alnum zijn...
ascii([]) -->
    [].
ascii([X|Xs]) -->
    [X],
    {
        code_type(X, ascii),
        X \== 32, % space
        X\== 10 % newline
    },
    ascii(Xs).

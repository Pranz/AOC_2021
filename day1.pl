:- use_module(library(pio)).
:- use_module(library(dcgs)).
:- use_module(library(iso_ext)).
:- use_module(library(lists)).
:- use_module(library(charsio)).

digit(D) --> [D], { char_type(D, hexadecimal_digit) }.

int([D|Ds]) --> digit(D), int(Ds).
int([D]) --> digit(D).

integer(X) --> int(S), { number_codes(X,S) }.

lines([X]) --> integer(X).
lines([X|Xs]) --> integer(X), "\n", !, lines(Xs).

increasing(X, Y) -->
    ...,
    [X],
    [Y],
    ...,
    {X < Y}.

look_ahead([T1, T2]), [T1, T2] --> [T1, T2].

window([]) --> [] | [_1] | [_1, _2].
window([Sum | Sums]) -->
    [A], look_ahead([B,C]),
    {Sum is A + B + C},
    window(Sums).

input_lines(Y) :- 
    phrase_from_file(lines(Y), './day1.input').

count_increasing(Count) :-
    input_lines(Input),
    findall((X,Y), phrase(increasing(X,Y), Input), Res),
    length(Res, Count).

count_increasing_window(Count) :-
    input_lines(Input),
    phrase(window(WInput), Input),
    findall((X, Y), phrase(increasing(X,Y), WInput), Res),
    length(Res, Count).
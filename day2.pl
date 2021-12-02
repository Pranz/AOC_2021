:- use_module(library(pio)).
:- use_module(library(dcgs)).
:- use_module(library(iso_ext)).
:- use_module(library(lists)).
:- use_module(library(charsio)).

digit(D) --> [D], { char_type(D, hexadecimal_digit) }.

int([D|Ds]) --> digit(D), int(Ds).
int([D]) --> digit(D).

command(forward) --> "forward".
command(down)    --> "down".
command(up)      --> "up".

integer(X) --> int(S), { number_codes(X,S) }.

lines([(C,X)]) --> command(C), " ", integer(X).
lines([(C,X)|Xs]) --> command(C), " ", integer(X), "\n", !, lines(Xs).

reduce(0, 0) --> [].
reduce(H, D) --> [(forward, HAdd)],
    reduce(PrevH, D),
    { H is HAdd + PrevH}.
reduce(H, D) --> [(down, DAdd)],
    reduce(H, PrevD),
    { D is PrevD + DAdd}.
reduce(H, D) --> [(up, DSub)],
    reduce(H, PrevD),
    { D is PrevD - DSub}.

reduce2([], Res, Res).
reduce2([(forward, HAdd) | RestInstr], (H, D, A), Output) :-
    NextD is D + A * HAdd,
    NextH is HAdd + H,
    reduce2(RestInstr, (NextH, NextD, A), Output).
reduce2([(down, AAdd) | RestInstr], (H, D, A), Output) :-
    NextA is A + AAdd,
    reduce2(RestInstr, (H, D, NextA), Output).
reduce2([(up, ASub) | RestInstr], (H, D, A), Output) :-
    NextA is A - ASub,
    reduce2(RestInstr, (H, D, NextA), Output).
    
input_lines(Y) :- phrase_from_file(lines(Y), './day2.input').

reduced_input(H, D, Mul) :-
    input_lines(Y),
    phrase(reduce(H, D), Y),
    Mul is H * D.

reduced_input2(H, D, A, Mul) :-
    input_lines(Instructions),
    reduce2(Instructions, (0,0,0), (H,D,A)),
    Mul is H * D.

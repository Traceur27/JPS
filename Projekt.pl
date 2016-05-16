% global variables list
variables([a,b,c,d,e,f,g,h,i,j,k,l,k,n,o,p,q,r,s,t,u,v,w,x,y,z]).

between(Min, _, Min).
between(Min, Max, Value) :-
    Cur is Min + 1,
    Cur =< Max,
    between(Cur, Max, Value).

pick_from_table_at([X|_], 1, X).
pick_from_table_at([_|R], J, Value) :-
	NewJ is J - 1,
	pick_from_table_at(R, NewJ, Value).

/**
 * insert_arg:
 *
 * - LastUsed - indeks ostatnio użytej zmennej w poprzednich wyrażeniach
 * - LastLocal - indeks ostatnio użytej zmennej
 * - FlagIn - czy wybrano do tej pory zmenną użytą poprzednio
 * - Value - zwrócona zmienna
 * - LastLocal - zwrócony największy indeks zmiennej użytej lokalnie
 * - FlagIn - czy udało się wybrać zmienną użytą poprzednio
 *
 * Wybranie zmiennej użytej w następniku lub we wcześniej zbudowanym wyrażeniu,
 * w poprzedniku - zakres indeksu od 1 do LastUsed; flag na 1
 */
insert_arg(LastUsed, LastLocal, FlagIn, Value, LastLocal, yes) :-
	between(1, LastUsed, Index),
	variables(Table),
	pick_from_table_at(Table, Index, Value).

/**
 * W przeciwnym przypadku przepisuję flagę
 */
insert_arg(LastUsed, LastLocal, FlagIn, Value, LastLocal, FlagIn) :-
	NewLastUsed is LastUsed + 1,
	between(NewLastUsed, LastLocal, Index),
	variables(Table),
	pick_from_table_at(Table, Index, Value).

/**
 * Wybranie nowej zmiennej - zwiększenie LastLocal o 1 i pobraniu zmiennej
 */
insert_arg(LastUsed, LastLocal, FlagIn, Value, NewLastLocal, FlagIn) :-
	NewLastLocal is LastLocal + 1,
	variables(Table),
	pick_from_table_at(Table, NewLastLocal, Value).


build_arg_list(N, vars(LastUsed, LastLocal), Flag, [Arg|Rest], RetLastUsed) :-
	N > 1,
	insert_arg(LastUsed, LastLocal, Flag, Arg, RetLastLocal, FlagOut),
    M is N - 1,
	build_arg_list(M, vars(LastUsed, RetLastLocal), FlagOut, Rest, RetLastUsed).

build_arg_list(1, vars(LastUsed, LastLocal), yes, [Arg], RetLastUsed) :-
	insert_arg(LastUsed, LastLocal, yes, Arg, RetLastUsed, FlagOut).

build_arg_list(1, vars(LastUsed, LastLocal), no, [Arg], RetLastUsed) :-
	insert_arg(LastUsed, LastLocal, no, Arg, RetLastUsed, yes).


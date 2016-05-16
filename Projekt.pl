between(Min, Max, Return) :- Cur is Min, Min < Max.
between(Min, Max, Min).
between(Min, Max, Value) :- Cur is Min + 1, Cur < Max, between(Cur, Max, Value).


pick_from_table_at([X|R], 0, X).
pick_from_table_at([X|R], J, Value) :-
	NewJ is J - 1,
	pick_from_table_at(R, NewJ, Value).



/** Wybranie zmiennej użytej w następniku lub we wcześniej zbudowanym wyrażeniu, w poprzedniku - zakres indeksu od 1 do LastUsed; flag na 1 */
insert_arg(LastUsed, LastLocal, FlagIn, Value, LastLocal, yes),
	between(1, LastUsed, Index),
	variable(Table),
	pick_from_table_at(Table, Index, Value).

/** W przeciwnym przypadku przepisuję flagę */
insert_arg(LastUsed, LastLocal, FlagIn, Value, LastLocal, FlagIn) :-
	NewLastUsed is LastUsed + 1,
	between(NewLastUsed, LastLocal, Index),
	variable(Table),
	pick_from_table_at(Table, Index, Value).

/** Wybranie nowej zmiennej - zwiększenie LastLocal o 1 i pobraniu zmiennej */
insert_arg(LastUsed, LastLocal, FlagIn, Value, LastLocal, FlagIn) :-
	NewLastLocal is LastLocal + 1,
	variable(Table),
	pick_from_table_at(Table, Index, Value).



build_arg_list(N, vars(LastUsed, LastLocal), Flag, [Arg|Rest], RetLastUsed) :-
	M is N + 1,
	M > 1,
	insert_arg(LastUsed, LastLocal, Flag, Arg, RetLastLocal, FlagOut),
	build_arg_list(M, vars(LastUsed, RetLastLocal), FlagOut, Rest, RetLastUsed).
build_arg_list(1, vars(LastUsed, LastLocal), yes, [LastArg|Rest], RetLastUsed) :-
	insert_arg(LastUsed, LastLocal, yes, Arg, LastArg, FlagOut).



% global variables list
variables([a,b,c,d,e,f,g,h,i,j,k,l,k,n,o,p,q,r,s,t,u,v,w,x,y,z]).

between(Min, Max, Min) :-
    Min =< Max.

between(Min, Max, Value) :-
    Min =< Max,
    Cur is Min + 1,
    Cur =< Max,
    between(Cur, Max, Value).

pick_from_table_at([X|_], 1, X).
pick_from_table_at([_|R], J, Value) :-
    J > 1,
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
 * Wybranie zmiennej użytej poprzednio w predykacie
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

covers(rule(Conseq, Anteced), Example) :-
    match_conseq(Conseq, Example, Bindings),
    match_anteced(Anteced, Bindings, _ ) .

match_conseq(Conseq, Example, BindingsOut) :-
    Conseq =.. [_|ArgList1],
    Example =.. [_|ArgList2],
    match_arg_lists(ArgList1,ArgList2,[],BindingsOut) .

match_anteced([ ], Bindings, Bindings) .
match_anteced([A|RestAnteced], BindingsIn, BindingsOut) :-
    match_expr(A, BindingsIn, Bindings1),
    match_anteced(RestAnteced, Bindings1, BindingsOut) .

known_fact(corka(janusz,grażyna)).
known_fact(corka(dżesika, grażyna)).
known_fact(corka(sebix, karyna)).
known_fact(corka(brajan, karyna)).

match_expr(Expr,BindingsIn,BindingsOut) :-
    known_fact(Fact),
    functor(Expr,Functor,N),
    functor(Fact,Functor,N),
    Expr =.. [_|ArgList1],
    Fact =.. [_|ArgList2],
    match_arg_lists(ArgList1,ArgList2,BindingsIn,BindingsOut) .

match_arg_lists([ ] ,[ ], Bindings, Bindings) .
match_arg_lists([Arg1|Rest1], [Arg2|Rest2], BindingsIn, BindingsOut) :-
    match_args(Arg1, Arg2, BindingsIn, Bindings1),
    match_arg_lists(Rest1, Rest2, Bindings1, BindingsOut) .

match_args(X,Y,[],[binding(X,Y)]):-!.
match_args(X,Y,[binding(X,Y)|Bindings],[binding(X,Y)|Bindings]):-!.
match_args(Arg1,Arg2,[binding(X,Y)|Bindings],[binding(X,Y)|NewBindings]):-
    Arg1\=X,
    Arg2\=Y,
    match_args(Arg1,Arg2,Bindings,NewBindings).


member1(X,[X|_]).
member1(X,[Y|Rest]) :-
    member1(X, Rest).

filter( Examples, Rule, Examples1) :-
    findall( Example, (member1(Example, Examples), covers(Rule, Example)), Examples1).


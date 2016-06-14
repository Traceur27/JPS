% baza wiedzy
known_fact(corka(karyna, andrzej)).
known_fact(corka(karyna, bożena)).
known_fact(corka(andżela, mirek)).
known_fact(corka(andżela, hanka)).
known_fact(syn(seba, janusz)).
known_fact(syn(seba, grażyna)).

known_fact(rodzic(andrzej, karyna)).
known_fact(rodzic(bożena, karyna)).
known_fact(rodzic(mirek, andżela)).
known_fact(rodzic(hanka, andżela)).
known_fact(rodzic(janusz, seba)).
known_fact(rodzic(grażyna, seba)).

% Procedury do opracowania
% match_args [OK]
% remove
% insert_arg [OK]
% build_arg_list [OK]
% suitable
% choose_best
% learn


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

% procedura do opracowania w ramach wykonania zadania
match_args(X,Y,[],[binding(X,Y)]):-!.
match_args(X,Y,[binding(X,Y)|Bindings],[binding(X,Y)|Bindings]):-!.
match_args(Arg1,Arg2,[binding(X,Y)|Bindings],[binding(X,Y)|NewBindings]):-
    Arg1\=X,
    Arg2\=Y,
    match_args(Arg1,Arg2,Bindings,NewBindings).

member1(X,[X|_]).
member1(X,[Y|Rest]) :-
    member1(X, Rest).

filter(Examples, Rule, Examples1) :-
    findall( Example, (member1(Example, Examples), covers(Rule, Example)), Examples1).


build_expr(LastUsed,Expr,RetLastUsed) :-
    predicate(Pred, N), % weź predykat N argumentowy
    build_arg_list(N, vars(LastUsed, LastUsed), false, ArgList, RetLastUsed), % zbuduj mu liste argumentow
    Expr =.. [Pred|ArgList]. % dołącz do Expr jako Pred(a,b,c,d,e)

candidate_rule(rule(Conseq, Anteced), PosExamples, NegExamples, LastUsed, rule(Conseq, [Expr|Anteced]), RetLastUsed) :-
    build_expr(LastUsed, Expr, RetLastUsed),
    suitable(rule(Conseq, [Expr|Anteced]), NegExamples).

scored_rule( PosExamples, NegExamples, PartialRule, LastUsed, rule_descr(CandPartialRule, Score, RetLastUsed) ) :-
    candidate_rule(PartialRule, PosExamples, NegExamples, LastUsed, CandPartialRule, RetLastUsed) ,
    filter( PosExamples, CandPartialRule, PosExamples1),
    filter( NegExamples, CandPartialRule, NegExamples1),
    length( PosExamples1, NPos),
    length(NegExamples1, NNeg),
    NPos > 0,
    Score is NPos - NNeg.

new_partial_rule( PosExamples, NegExamples, PartialRule, LastUsed, BestRule, RetLastUsed) :-
    findall(NewRuleDescr, scored_rule( PosExamples, NegExamples, PartialRule, LastUsed, NewRuleDescr) ,Rules),
    choose_best( Rules, BestRule, RetLastUsed).

learn_one_rule( _ , [ ] , Rule, _ , Rule).
learn_one_rule( PosExamples, NegExamples, PartialRule, LastUsed, Rule ) :-
    new_partial_rule( PosExamples, NegExamples, PartialRule ,LastUsed, NewPartialRule, NewLastUsed) ,
    filter( PosExamples, NewPartialRule, PosExamples1),
    filter( NegExamples, NewPartialRule, NegExamples1),
    learn_one_rule( PosExamples1, NegExamples1, NewPartialRule, NewLastUsed, Rule ) .

learn_rules( [ ] , _ , _ , _ , [ ] ) .
learn_rules(PosExamples, NegExamples, Conseq, VarsIndex, [Rule | RestRules]) :-
    learn_one_rule( PosExamples, NegExamples, rule(Conseq, [ ]), VarsIndex, Rule ) ,
    remove( PosExamples, Rule, RestPosExamples),
    learn_rules(RestPosExamples, NegExamples, Conseq, VarsIndex, RestRules) .


suitable(rule(Conseq, [Expr|Anteced]), [E|NegExamples]) :-
    not(covers(Conseq, [Expr|Anteced]), E), !.

suitable(rule(Conseq, [Expr|Anteced]), [E|NegExamples]) :-
    suitable(rule(Conseq, [Expr|Anteced]), NegExamples).

choose_best([F|Rulesd], BestRule, RetLastUsed) :-
    choose_best_iterator(Rulesd, F, rule_descr(BestRule, Score, RetLastUsed)).

choose_best_iterator([], BestRule, BestRule).

choose_best_iterator([rule_descr(CandPartialRule, Score, RetLastUsed)| Rest], rule_descr(CurrCandPartialRule, CurrScore, CurrRetLastUsed), Best) :-
    Score > CurrScore,!,
    choose_best_iterator(Rest, rule_descr(CandPartialRule, Score, RetLastUsed), Best).

choose_best_iterator([rule_descr(CandPartialRule, Score, RetLastUsed)| Rest], rule_descr(CurrCandPartialRule, CurrScore, CurrRetLastUsed), Best) :-
    choose_best_iterator(Rest, rule_descr(CurrCandPartialRule, CurrScore, CurrRetLastUsed), Best).

remove([], _, []).

remove([E|RestExamples], Rule, RestPosExamples) :-
    covers(Rule, E),!,
    remove(RestExamples, Rule, RestPosExamples).

remove([E|RestExamples], Rule, [E|RestPosExamples]) :-
    remove(RestExamples, Rule, RestPosExamples).

learn(ResRules) :-
    functor(ResRules, Name, N),
    % przykłady pozytywne
    findall(P, select_pos_examples(ResRules, P), PosExamples),
    write(PosExamples),
    get_negative_examples(PosExamples, NegExamples),
    % przykłady negatywne
    % user input
    learn_rules(PosExamples, NegExamples, x, 1, ResRules).

%corka(x,y)
select_pos_examples(Template, ArgList) :-
    known_fact(Fact),
    functor(Fact, Name, N),
    functor(Template, Name, N),
    Fact =.. [_|ArgList].


main :-
    learn(corka(x,y)),
    write(R).

:- initialization(once(((main ; true), halt))).

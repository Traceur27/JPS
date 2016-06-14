% Baza wiedzy 2
known_fact(syn(tomek,adam)).
known_fact(syn(adam,wojtek)).
known_fact(wnuk(tomek,wojtek)).

known_fact(syn(marcin,piotr)).
known_fact(syn(piotr,jerzy)).
known_fact(wnuk(marcin,jerzy)).

known_fact(syn(krzysztof,wiktor)).
known_fact(syn(wiktor,bohdan)).
known_fact(wnuk(krzysztof,bohdan)).

known_fact(syn(tomeka,adama)).
known_fact(syn(adama,wojteka)).
known_fact(wnuk(tomeka,wojteka)).

known_fact(syn(marcina,piotra)).
known_fact(syn(piotra,jerzya)).
known_fact(wnuk(marcina,jerzya)).

known_fact(syn(janusz,anna)).
known_fact(corka(anna,mariusz)).
known_fact(wnuk(janusz,mariusz)).

known_fact(syn(adrian,magda)).
known_fact(corka(magda,michal)).
known_fact(wnuk(adrian,michal)).

/*known_fact(syn(krzysztofa,wiktora)).
known_fact(syn(wiktora,bohdana)).
known_fact(wnuk(krzysztofa,bohdana)).

known_fact(syn(tomekab,adamab)).
known_fact(syn(adamab,wojtekab)).
known_fact(wnuk(tomekab,wojtekab)).

known_fact(syn(marcinab,piotrab)).
known_fact(syn(piotrab,jerzyab)).
known_fact(wnuk(marcinab,jerzyab)).

known_fact(syn(krzysztofab,wiktorab)).
known_fact(syn(wiktorab,bohdanab)).
known_fact(wnuk(krzysztofab,bohdanab)). */


predicate(syn,2).
predicate(corka,2).

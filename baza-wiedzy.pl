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

known_fact(babcia(genowefa, seba)).
known_fact(dziadek(stefan, seba)).
known_fact(rodzic(stefan, janusz)).
known_fact(rodzic(genowefa, janusz)).
known_fact(syn(janusz, stefan)).
known_fact(syn(janusz, genowefa)).


known_fact(babcia(marcelina, seba)).
known_fact(dziadek(edmund, seba)).
known_fact(rodzic(edmund, grażyna)).
known_fact(rodzic(marcelina, grażyna)).
known_fact(corka(grazyna, marcelina)).
known_fact(corka(grazyna, edmund)).

known_fact(rodzic(adam, andrzeja)).
known_fact(rodzic(ala, andrzeja)).
known_fact(syn(andrzej, adam)).
known_fact(syn(andrzej, ala)).

known_fact(rodzic(ola, bożena)).
known_fact(rodzic(marek, bożena)).
known_fact(córka(bożena, marek)).
known_fact(córka(bożena, ola)).

predicate(corka, 2).
predicate(rodzic, 2).
predicate(syn, 2).

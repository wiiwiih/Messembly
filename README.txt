Kääntäjätekniikan harjoitustyö "Messembly"
tekijät: Isoaho, Janne & Lappi, Vilma

YLEINEN
Alkuperäisestä suunnitelmasta poiketen Messemblyn toteutus jäi tämän kurssin puitteissa kesken. Tarkemmin sanottuna Messemblylle on
toteutettu toimiva sanastin/jäsennin, semanttinen analyysi (tyypin tarkastus) ja suora tulkki, mutta ei Webassembly-kääntäjää.

TYÖNJAKO
Työtä tehtiin aina molempien läsnäollessa sekä osallistuessa aktiivisesti. Ryhmän jäsenet olivat koko ajan tietoisia
harjoitustyöntilasta ja tekivät päätökset yhdessä.  Viimeistelyvaiheessa Vilma otti päävastuun semanttisen analyysin viimeistelyssä
ja Janne hoiti viimeisten muutosten suunnittelun ja toteuksen jäsentimeen ja tulkkiin.

APUOHJELMAT
Koodin ajamiseen tarvitaan stack-ohjelmisto. (Myös muiden Haskell-työkalujen käyttö lienee mahdollista, mutta ohjeet on tehty olettaen,
että käytössä on stack.)

TYÖN KÄÄNTÄMINEN
Työn kääntämiseen tulee käyttäjän mennä komentorivillä (tai terminaalissa) hakemistoon ~/Messembly/Kääntäjä/Messer/ ja
ajaa komento "stack build". Saattaa olla mahdollista, että käyttäjän on ensin ajettava "stack init" -komento.

OHJEET KÄÄNTÄJÄN KÄYTTÖÖN
Tulkkia voi käyttää 1. joko stackin kautta "stack exec Messer 'tulkattava tiedosto' 'arg1' 'arg2' jne." (ilman '- tai "-merkkejä)
                    2. käyttämällä käännettyä "messer.exe" tiedostoa ja antamalla sille argumentit samalla tavalla alkaen 
                        tiedoston nimestä
Tulkkia ajettaessa tulkattavan tiedoston nimi ON AINA pakollinen, mutta argumentit (arg1 jne.) riippuvat 
tulkattavan tiedoston mainin parametreista. Mikäli haluaa antaa komentoriviparametrejä jotka alkavat "-"-merkillä esimerkiksi 
negatiiviset luvut tai merkkijonot, jotka alkavat viivalla, tulee käyttää messer.exe:ä, koska stack tulkitsee ne optioiksi. 
Komentoriviargumenteissa bool-tyyppiset arvot ovat joko "True" tai "False" (isolla alkukirjaimella!).

Esimerkki kääntäjän käytöstä stack execin avulla:
1. Käännä työ "stack build" -komennolla
2. Aja "stack exec Messer test.mess 5", tässä test.mess on ~/Messembly/Kääntäjä/Messer/ -kansiossa oleva testitiedosto, joka laskee
    fibonaccin n:nnen luvun. Esimerkki komennossa n on annettu parametri 5.
3. Mikäli tulkkaus onnistuu ongelmitta, kertoo ohjelmisto sen tulkkauksen päätteeksi viestillä "Ohjelman suoritus onnistui"
    muissa tapauksissa jäsennin/semanttinen analyysi/tulkki antaa virheilmoituksen.

Messembly-kielen syntaksi on määritelty ~/Messembly/Dokumentaatio/Messembly_syntaksi.txt tiedostossa 
ja lopullinen kielioppi tiedostossa ~/Messembly/Dokumentaatio/Kielioppi/Lopullinen_kielioppi.txt. Lisää esimerkkikoodeja löytyy
kansiosta ~/Messembly/TestiKoodit/.

Messemblyllä kirjoitetut tiedostot tulee oikean konvention mukaan tallentaa päätteellä ".mess", vaikkakaan tulkki
ei siitä välitä.

TESTIT
Sanastimella/Jäsentimelle on pieni joukko testejä, jotka voi helpoiten ajaa seuraavasti:
    Siirry komentorivillä kansioon ~/Messembly/Kääntäjä/Messer/ ja aja komento "stack ghci src/MesserTests.hs"
    Tällöin käynnistyy Haskell tulkki ghci, josta voi kutsua testit ajavaa metodia kirjoittamalla "ajaTestit"
    HUOM: On tärkeää, että käyttäjä on juuri tässä hakemistossa, koska testit ajava ohjelma etsii testejä suhteessa
    käyttäjän sen hetkiseen kansioon.

Testausta on toteutettu suunniteltua vähemmän johtuen aikataulutuksellisista haasteista. Olemassa olevat testit kuitenkin tarkistavat 
jäsentyvätkö ~/Messembly/TestiKoodit/ -kansiossa olevat .mess tiedostot oikein.

VAADITUT TIEDOSTOT
Messemblyn käyttö ei vaadi muita tiedostoja, jos stack toimii normaalisti ja lataa .cabal-tiedostossa olevat dependencyt oikein. 
Mikäli tämä tarvii tehdä manuaalisesti tulee seuraavat dependencyt olla kääntämistä varten:
    megaparsec, parser-combinators, tasty, tasty-hunit, directory, containers.
# Projekt 2

Celem drugiego projektu jest zbudowanie asystenta dla R, ułatwiającego wczytanie, czyszczenie i wstępne przetwarzanie danych. 

## Motywacja:

Od kilku lat ton dla rozwoju R nadają głównie informatycy z RStudio (tidyverse). Aby udowodnić, że R nie jest tylko DSL (domain specific language) wprowadza się różne rozwiązania upodobniające prace z R do pracy z innymi językami programowania. Ale nie zawsze tak było. Rozwojem R od 1997 kieruje R Core Team, zespół złożony tak ze statystyków jak i informatyków. Zespół ten przejął rozwój R od ojców założycieli – Roberta Gentelmana i Rossa Ihaki (rozwój od 1992).
Ale Robert i Ross oparli R o język S, który był rozwijany w laboratoriach Bell Labs od 1975. Co ciekawe to w początkach eSa widać zamysł czym powinien być język pracy dla analizy danych.

Bell Labs to legendarny oddział badawczy AT&T (kiedyś monopolisty w US). Został wpisany na listę rekordów Guinessa ze względu na liczbę noblistów, którzy tam pracowali. W Bell Lab wymyślono Unixa, C, C++, tranzystor czy język S. W tym oddziale nad językiem S do analizy danych pracował John Chambers (kolejna legenda, specjalista od statystyki obliczeniowej) i Richard Becker (specjalista od interaktywnego przetwarzania). Ich prace były pod silnym wpływem Johna Tukeya (ojciec EDA, matematyk i statystyk). Jednym z zamysłów eSa było interaktywne przetwarzanie. Konkurencja (głównie SAS) opierała analizy o duże konfigurowalne bloki, które jako wynik generowały długie raporty zawierające wszystkie możliwe wyniki. S był inny, wyniki otrzymywało się w trakcie eksploracji / konwersacji z danymi.

Spróbujemy pociągnąć dalej ten wątek rozwoju narzędzi do analizy danych. W ramach trzeciego projektu zbudują państwo pakiet hugo, który będzie wirtualnym asystentem analityka. Każda osoba będzie odpowiedzialna za dodanie jednej, względnie małej, funkcjonalności. Pomimo iż ta funkcjonalność będzie mała, to zadanie nie jest proste z dwóch powodów.

*	Nacisk położony jest na jakość a nie ilość. Zamiast pisać setki linii kodu, może się okazać że implementacja tego to jedna strona kodu. Ale za tą stroną pójdą dwie strony testów i trzy strony dokumentacji. 
*	Każda osoba przygotowuje jedną funkcjonalność, ale funkcjonalności różnych osób muszą ze sobą współpracować i być spójne. Muszą państwo uzgodnić spójny system nazywania zmiennych, wykorzystania pakietów zależnych, interakcji z użytkownikiem.

Docelowy pakiet z państwa funkcjami powinien znaleźć się w repozytorium
https://github.com/hugo4r/hugo
(będę dodawał państwu uprawnienia do tego repozytorium).

## Ocena

Indywidualne punkty z tego zadania wyliczone będą według wzoru (iloczyn pozwoli skupić się na jakości):
50 pkt x  składowa1 x składowa2 x składowa3 x  składowa4

*	składowa1: jakość kodu (indywidualnie). Kod powinien być czytelny, nazwy zmiennych zrozumiałe, nie należy stosować krótkich, nic nie znaczących nazw (x, i, a itp.). Kod powinien być wyposażony w testy (test that) które go pokrywają w przynajmniej 95%.
*	składowa2: jakoś dokumentacji (indywidualnie). Funkcja powinna mieć czytelną dokumentację w języku angielskim z jasno opisanym celem działania, opisanymi argumentami, działającymi przykładami. Funkcja powinna komunikować się z użytkownikiem w czytelny sposób.
*	składowa3: spójność kodu z innymi funkcjami (porównawczo). Pomimo iż każdy tworzy tylko jedną funkcję, to wszystkie funkcje jako całość powinny być możliwe w użyciu w sposób spójny. Jakikolwiek sposób komunikacji państwo wybiorą, w tym kryterium oceniał będę spójność państwa funkcji z wszystkimi pozostałymi.
*	składowa4: wygoda i możliwości oferowane przez funkcjonalność (indywidualnie). Opracowana funkcjonalność powinna wychodzić naprzeciw oczekiwaniom użytkownika. 

Uwaga, w tworzeniu kodu, nie należy (o ile to nie jest absolutnie konieczne) używać niestandardowych pakietów poza pakietami bazowymi i pakietami z tidyverse.

## Architektura

Pakiet hugo posiada funkcje o nazwach hugo_xxx_yyy() w których użytkownik prosi asystenta o wykonanie xxx_yyy. Praca zaczyna się od rozpoczęcia badania (investigation). Wszystkie wyniki, wygenerowane w ramach badania, są zapisywane we wskazanym katalogu (recordings).

Funkcje hugo mogą być używane bez dziesiątek argumentów. Ważne wartości argumentów hugo próbuje sam odgadnąć. Jeżeli się nie uda, to zapyta się użytkownika. Zapamięta odpowiedzi użytkownika i nie będzie pytał się dwa razy o to samo.


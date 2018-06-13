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

## Funkcjonalności

Proszę aby każdy przypisał się do jednej

* `hugo_start_investigation(path, ...)` – ta funkcja tworzy katalog roboczy badania. W nim należy zapisywac wszystkie istotne zmienne. Ta funkcja już istnieje, implementacja: PBi.
* `hugo_save_investigation(...)` – ta funkcja zapisuje stan wszystkich ważnych zmiennych i pakietów w katalogu path. Powinna współpracować z hugo_continue_investigation()    --- monikachudek
* `hugo_continue_investigation(path, ...)` -  ta funkcja odczytuje z katalogu badania, do sesji wszystkie ważne zmienne i ładuje potrzebne pakiety by kontynuować badanie (wczytuje pakiety w wersjach aktualnych w chwili gdy badanie było zapisane).   -- Mateusz Kobyłka
* `hugo_read_data(path, ...)` – ta funkcja wczytuje dane z pliku path (może być url). Powinna obsługiwać przynajmniej pliki txt, csv, rda, RData, json, xls, xls, tsv. Tam gdzie to możliwe powinna odgadywać separator i nagłówki. Jeżeli nie uda się odgadnąć formatu danych to funkcja powinna wyświetlić kilka wierszy i wypytać użytkownika co jest separatorem czy jest nagłówek itp. Wynikowe dane zapisywane są w katalogu data badania   --- komosinskid
* `hugo_clean_data(data, ...)` - ta funkcja imputuje wartości brakujące medianą lub modą (dla factorów), w zmiennych skośnych identyfikuje wartości odstające i przycina do rozsądnego kwantyla, zmienne factorowe spłasza, łącząc najrzadziej występujące factor w jeden   --- Eliza K
* `hugo_summarise_data(data, ...)` tworzy raport w postaci pliku html/pdf opisujący rozkłady poszczególnych zmiennych. Tak jak w pakiecie https://github.com/ekstroem/dataMaid  ---KasiaWo
* `hugo_share_object(obj)` wybrany obiekt zapisuje do wskazanego repozytorium na github a jako wynik zwraca instrukcję pozwalającą na pobranie wskazanego obiektu do konsoli R (np. przez współpracownika)  --MatKru
* `hugo_share_secret(obj, secret)`, działa jak hugo_share_object i może z niej korzystać, ale dodatkowo obiekt jest szyfrowany kluczem symetrycznym secret. Przez co na Githubie jest przechowywany w postaci zaszyfrowanej --- romaszkok
* `hugo_memorise_plot(plot)` w katalogu badania w podkatalogu gallery zapamiętywany jest wykres jako obiekt R w formacie rda, razem z miniaturką w formatach pdf/png, które pozwalają wykres łatwiej wyszukać  --- golawskaj
* `hugo_memorise(obj)` w katalogu badania w podkatalogu memory, zapisywane są wskazne obiekty R w postaci plików rda  --- AnnaGierlak
* `hugo_show_history()` każda z funkcji w pakiecie hugo powinna dbać o to by w katalogu badania była zapisywana aktualna historia poleceń, czyli wszystkie funkcje `hugo_*` muszą być wzbogacone o coś co tą historie zapisze. Ta funkcja wyświetla wzbogaconą historię poleceń. Wzbogacenie polega na tym, że jeżeli ktoś użył polecenia hugo_memorise* to obok komendy trzymana jest też względna ścieżka do wynikowego pliku a przy `hugo_memorise_plot()` trzymany jest link do miniaturki (wynikiem może być seria wpisów w konsoli lub strona HTML) .---lgelmo
* `hugo_show_statistics()` wyświetla podsumowania dla obecnego badania, od kiedy jest prowadzone, ile obiektów zapamiętano, ile zbiorów danych wczytano, ile instrukcji wykonano  --- jablonskaj
* `hugo_train_model(data, formula)` dla zadanej formuły i danych buduje modele regresyjne, drzewa decyzyjne, gbmlight, zapisuje modele do katalogu models a jako wynik zwraca wytrenowane modele oraz ocenę jak te modele są dobre  --- kozaka93
* `hugo_memorise_table(table)` w katalogu badania w podkatalogu gallery zapamiętywana jest tabela jako obiekt R w formacie rda, razem z plikiem md z tabelą w postaci markdown i tabelą zapisaną w formacie xlsx i docx --- Maria Piliszek
* `hugo_memorise_model(model)` w katalogu badania w podkatalogu gallery zapamiętywany jest model jako obiekt R w formacie rda, razem z plikiem md podsumowującym model (w oparciu o statystyki wyznaczane przez pakiet `broom`) --- MalgosiaL


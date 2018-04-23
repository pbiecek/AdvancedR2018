# Programowanie i analiza danych w R dla Zaawansowanych

Zanim zaczniemy:

https://goo.gl/forms/rZPuFVsXgVECY2403


Plan spotka&#324; MINI PW
-------------------------

* 14 III [scrapping](https://pbiecek.gitbooks.io/przewodnik/content/Programowanie/jak_wczytywac_korpusy_tekstu.html)
* 21 III [Tidyverse cz 1](https://pbiecek.gitbooks.io/przewodnik/content/Programowanie/czyscic_przetwarzac.html) a dokładniej `dplyr` i `tidyr`
* 28 III [Budowa pakietów](https://pbiecek.gitbooks.io/przewodnik/content/Programowanie/pakiety/po_co.html)
* 4 IV [Shiny](https://pbiecek.gitbooks.io/przewodnik/content/Programowanie/jak_tworzyc_aplikajce.html)
* 11 IV [HPC](https://rawgit.com/pbiecek/RandBigData/master/MINI_2015/materialy/hpc/hpc.html) + [Archivist](https://pbiecek.gitbooks.io/przewodnik/content/Programowanie/pazury/archivist.html)
* 18 IV Oddanie projektu 1
* 25 IV Debugowanie kodu, profilowanie kodu
* 9 V Prezentacje pakietów cz. 1
* 16 V Prezentacje pakietów cz. 2
* 23 V Tidyverse cz 2, broom, forcats, purrr
* 30 V Bazy danych duże i małe
* 6 VI TBA
* 13 VI Oddanie projektu 2


Projekt 1
---------

Projekt 1 można wykonywać w grupach do 3 osób.
Celem projektu jest zebranie z różnych źródeł skryptów R od różnych autorów, a następnie wykonanie prostej analizy częstościowej na tych skryptach.
Każdy zespół powinien:

1. Z repozytoriów kodu R (takich jak GitHub, CRAN, Bioconductor inne) pobrać kody instrukcji R dla przynajmniej 10 różnych autorów (zalecana liczba to 100+ różnych autorów). Kody powinny być zbierane w postaci plików tekstowych z podziałem na dwie grupy: A) kody R dla pakietów i B) kody R dla skryptów/analiz/raportów.
2. Po zebraniu kodów dla każdego autora/grupy należy przeprowadzić analizę dotyczącą częstości wykorzystania różnych pakietów R/funkcji R.
3. Należy zaprezentować analizę porównawczą kodów od różnych autorów/grup.

Wśród zebranych skryptów należy mieć przynajmniej 10 autorów/źródeł których nie ma żadna inna grupa.

Punkt 3 będzie prezentowany na zajęciach. Zaliczenie tego projektu jest na podstawie zaprezentowanego raportu oraz wolumenu zebranych kodów R.

Można w analizach wykorzystywać kody zebrane przez inne zespoły (za ich zgodą), ale każdy zespół musi dostarczyć przynajmniej 10 unikatowych źródeł.

Jako ciekawe rozszerzenie projektu można potrafktować analizy związane ze sposobem nazywania zmiennych/funkcji.

Wyniki (prezentacje, skrypty scapujące dane) proszę umieścić w katalogu `Projekt1` w podkatalogu z nazwą zespołu. Spakowane pozyskane skrypty R należy umieścić w portalu typu WeTransfer, Dropbox, GDrive. W ww katalogu wystarczy umieścić link do tych materiałów.


Prezentacje pakietów
--------------------

W niewielkich grupach (jedna lub dwie osoby) proszę przygotowac krótką prezentację nt jednego wybranego pakietu dla R. Prezentacja powinna być krótka (10 min) zawierać informacje o tym: 1. jaki problem rozwiązuje dany pakiet, 2. przykład użycia danego pakietu, 3. dyskusja nt elastyczności i łatwości użycia danego pakietu.

Pakiety należy omawiać w oparciu o artykułu z JSS. Lista pakietów do wyboru znajduje się poniżej. Do jednego tematu zgłosić może się maksymalnie jedna grupa, decyduje kolejność zgłoszeń, proszę dopisać się poniżęj do tematu i przesłać zgłoszenie jako pull request.

Prezentacja powinna trwać do 10 min, ale należy przewidzieć kolejne 10 min na dyskusje / samodzielne uruchomienie pakietu przez uczestników zajęć (do prezentacji należy dołączyć kod R lub umieścić go na slajdach w prezentacji).

Tematy:

* [Visually Exploring Missing Values in Multivariable Data Using a Graphical User Interface](https://www.jstatsoft.org/article/view/v068i06) Xiaoyue Cheng, Dianne Cook, Heike Hofmann *--AK--*
* [missMDA: A Package for Handling Missing Values in Multivariate Data Analysis](https://www.jstatsoft.org/article/view/v070i01) Julie Josse, François Husson *--MŁ--*
* [The R Package groc for Generalized Regression on Orthogonal Components](https://www.jstatsoft.org/article/view/v065i01) Martin Bilodeau, Pierre Lafaye de Micheaux, Smail Mahdi
* [fitdistrplus: An R Package for Fitting Distributions](https://www.jstatsoft.org/article/view/v064i04) Marie Laure Delignette-Muller, Christophe Dutang
* [ranger: A Fast Implementation of Random Forests for High Dimensional Data in C++ and R](https://www.jstatsoft.org/article/view/v077i01) Marvin N. Wright, Andreas Ziegler *--Kam Rom*
* [Interactive Dendrograms: The R Packages idendro and idendr0](https://www.jstatsoft.org/article/view/v076i10) Tomáš Sieger, Catherine B. Hurley, Karel Fišer, Claudia Beleites      *--Mat Kru*
* [CircNNTSR: An R Package for the Statistical Analysis of Circular, Multivariate Circular, and Spherical Data Using Nonnegative Trigonometric Sums](https://www.jstatsoft.org/article/view/v070i06) Juan José Fernández-Durán, María Mercedes Gregorio-Domínguez
* [bartMachine: Machine Learning with Bayesian Additive Regression Trees](https://www.jstatsoft.org/article/view/v070i04) Adam Kapelner, Justin Bleich
* [gramEvol: Grammatical Evolution in R](https://www.jstatsoft.org/article/view/v071i01) Farzad Noorian, Anthony M. de Silva, Philip H. W. Leong
* [R Package gdistance: Distances and Routes on Geographical Grids](https://www.jstatsoft.org/article/view/v076i13) Jacob van Etten
* [Identifying Causal Effects with the R Package causaleffect](https://www.jstatsoft.org/article/view/v076i12) Santtu Tikka, Juha Karvanen
* [PrevMap: An R Package for Prevalence Mapping](https://www.jstatsoft.org/article/view/v078i08) Emanuele Giorgi, Peter J. Diggle
* [pvclass: An R Package for p Values for Classification](https://www.jstatsoft.org/article/view/v078i04) Niki Zumbrunnen, Lutz Dümbgen
* [SmoothHazard: An R Package for Fitting Regression Models to Interval-Censored Observations of Illness-Death Models](https://www.jstatsoft.org/article/view/v079i07)
Célia Touraine, Thomas A. Gerds, Pierre Joly
* [Constructing Multivariate Survival Trees: The MST Package for R](https://www.jstatsoft.org/article/view/v083i12) Peter Calhoun, Xiaogang Su, Martha Nunn, Juanjuan Fan
* [A Recipe for inferference: Start with Causal Inference. Add Interference. Mix Well with R.](https://www.jstatsoft.org/article/view/v082i02) Bradley C. Saul, Michael G. Hudgens
* [SIS: An R Package for Sure Independence Screening in Ultrahigh-Dimensional Statistical Models](https://www.jstatsoft.org/article/view/v083i02) Diego Franco Saldana, Yang Feng
* [PPtreeViz: An R Package for Visualizing Projection Pursuit Classification Trees](https://www.jstatsoft.org/article/view/v083i08) Eun-Kyung Lee
* [mplot: An R Package for Graphical Model Stability and Variable Selection Procedures](https://www.jstatsoft.org/article/view/v083i09) Garth Tarr, Samuel Müller, Alan H. Welsh *-- Katarzyna W*
* [epinet: An R Package to Analyze Epidemics Spread across Contact Networks](https://www.jstatsoft.org/article/view/v083i11) Chris Groendyke, David Welch
* [ThresholdROC: Optimum Threshold Estimation Tools for Continuous Diagnostic Tests in R](https://www.jstatsoft.org/article/view/v082i04) Sara Perez-Jaume, Konstantina Skaltsa, Natàlia Pallarès, Josep L. Carrasco
* [tscount: An R Package for Analysis of Count Time Series Following Generalized Linear Models](https://www.jstatsoft.org/article/view/v082i05) Tobias Liboschik, Konstantinos Fokianos, Roland Fried
* [vdmR: Generating Web-Based Visual Data Mining Tools with R](https://www.jstatsoft.org/article/view/v082i06) Tomokazu Fujino *-- Eliza K* 
* [trackeR: Infrastructure for Running and Cycling Data from GPS-Enabled Tracking Devices in R](https://www.jstatsoft.org/article/view/v082i07) Hannah Frick, Ioannis Kosmidis --Monika Chudek, Anna Gierlak
* [Computing and Visualizing Dynamic Time Warping Alignments in R: The dtw Package](https://www.jstatsoft.org/article/view/v031i07) Toni Giorgino --komosinskid


Zaliczenie
----------

Zaliczenie jest oparte o 

* zespołowy projekt 1 (30% zaliczenia), 
* prezentacje pakietów (20% zaliczenia),
*  indywidualny projekt 2 (50% zaliczenia).


Materiały dodatkowe
-------------------

## Scrapping

```
library("BetaBit")
proton()

------

w80dni <- readLines("http://www.gutenberg.org/cache/epub/103/pg103.txt")
head(w80dni)

------

library("rvest")
premiery <- read_html("http://www.filmweb.pl/premiere")
filmy <- html_nodes(premiery, ".filmPreview__title")
html_text(filmy)
```

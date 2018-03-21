# Programowanie i analiza danych w R dla Zaawansowanych

Zanim zaczniemy:

https://goo.gl/forms/rZPuFVsXgVECY2403


Plan spotka&#324; MINI PW
-------------------------

* 14 III [scrapping](https://pbiecek.gitbooks.io/przewodnik/content/Programowanie/jak_wczytywac_korpusy_tekstu.html)
* 21 III [Tidyverse cz 1](https://pbiecek.gitbooks.io/przewodnik/content/Programowanie/czyscic_przetwarzac.html) a dokładniej `dplyr` i `tidyr`
* 28 III Budowa pakietów
* 4 IV Shiny
* 11 IV HPC + Archivist
* 18 IV Oddanie projektu 1
* 9 V Prezentacje pakietów cz. 1
* 16 V Prezentacje pakietów cz. 2
* 23 V Tidyverse cz 2, broom, forcats, purrr
* 30 V TBA
* 6 VI TBA
* 13 VI Oddanie projektu 2





Materiały
---------

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

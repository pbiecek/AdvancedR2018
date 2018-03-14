# Programowanie i analiza danych w R dla Zaawansowanych

Zanim zaczniemy:

https://goo.gl/forms/rZPuFVsXgVECY2403


Plan spotka&#324; MINI PW:
-------------------------

* 14 III [scrapping](https://pbiecek.gitbooks.io/przewodnik/content/Programowanie/jak_wczytywac_korpusy_tekstu.html)

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

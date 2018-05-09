library(PogromcyDanych)
library(dplyr)


#1   Która Marka występuje najczęściej w zbiorze danych auta2012?
auta2012 %>% 
  group_by(Marka) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>%
  head(3)
#2   Spośród aut marki Toyota, który model występuje najczęściej.
auta2012 %>% 
  filter(Marka == "Toyota") %>%
  group_by(Model) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>%
  head(3)
#3   Sprawdź ile jest aut z silnikiem diesla wyprodukowanych w 2007 roku?
auta2012 %>% 
  filter(Rodzaj.paliwa == "olej napedowy (diesel)", Rok.produkcji == 2007) %>%
  group_by() %>%
  summarise(n = n()) 
#4   Jakiego koloru auta mają najmniejszy medianowy przebieg?
auta2012 %>% 
  group_by(Kolor) %>%
  filter(!is.na(Przebieg.w.km)) %>%
  summarise(median_przeb = median(Przebieg.w.km)) %>% 
  arrange(median_przeb) %>%
  head(3)
#5   Gdy ograniczyć się tylko do aut wyprodukowanych w 2007, która Marka występuje najczęściej w zbiorze danych auta2012?
auta2012 %>% 
  filter(Rok.produkcji == 2007) %>%
  group_by(Marka) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>%
  head(3)
#6   Spośród aut marki Toyota, który model najbardziej stracił na cenie pomiędzy rokiem produkcji 2007 a 2008.
auta2012 %>% 
  filter(Marka == "Toyota" & (Rok.produkcji == 2007 | Rok.produkcji == 2008)) %>%
  group_by(Model, Rok.produkcji) %>% 
  summarise(srCena = mean(Cena.w.PLN)) %>% 
  spread(Rok.produkcji, srCena) %>%
  mutate(roznica = `2008` - `2007`) %>%
  arrange(roznica) %>%
  select(Model, roznica)
#7   Spośród aut z silnikiem diesla wyprodukowanych w 2007 roku która marka jest najdroższa?
auta2012 %>% 
  filter(Rok.produkcji == 2007, Rodzaj.paliwa == "olej napedowy (diesel)") %>%
  group_by(Marka) %>% 
  summarise(srCena = mean(Cena.w.PLN)) %>% 
  arrange(desc(srCena)) %>%
  head(3)
#   Ile jest aut z klimatyzacją?
auta2012 %>% 
  mutate(ma_klime = grepl("klimatyzacja", as.character(Wyposazenie.dodatkowe))) %>%
  group_by(ma_klime) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>%
  head(3)
#   Gdy ograniczyć się tylko do aut z silnikiem ponad 100 KM, która Marka występuje najczęściej w zbiorze danych auta2012?
auta2012 %>% 
  filter(KM > 100) %>%
  group_by(Marka) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>%
  head(3)
#   Spośród aut marki Toyota, który model ma największą różnicę cen gdy porównać silniki benzynowe a diesel?
auta2012 %>% 
  filter(Marka == "Toyota") %>%
  group_by(Model, Rodzaj.paliwa) %>% 
  summarise(srCena = mean(Cena.w.PLN)) %>% 
  spread(Rodzaj.paliwa, srCena) %>%
  mutate(roznica = `olej napedowy (diesel)` - `benzyna`) %>%
  arrange(roznica) %>%
  select(Model, roznica)
#   Spośród aut z silnikiem diesla wyprodukowanych w 2007 roku która marka jest najtańsza?
auta2012 %>% 
  filter(Rok.produkcji == 2007, Rodzaj.paliwa == "olej napedowy (diesel)") %>%
  group_by(Marka) %>% 
  summarise(srCena = mean(Cena.w.PLN)) %>% 
  arrange(srCena) %>%
  head(3)
#   W jakiej marce klimatyzacja jest najczęściej obecna?
auta2012 %>% 
  mutate(ma_klime = grepl("klimatyzacja", as.character(Wyposazenie.dodatkowe))) %>%
  group_by(Marka, ma_klime) %>% 
  filter(ma_klime == TRUE) %>%
  summarise(n = n(), mean_klima = mean(ma_klime)) %>% 
  arrange(desc(mean_klima)) %>%
  head(3)
#   Gdy ograniczyć się tylko do aut o cenie ponad 50 000 PLN, która Marka występuje najczęściej w zbiorze danych auta2012?
auta2012 %>% 
  filter(Cena.w.PLN > 50000) %>%
  group_by(Marka) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>%
  head(3)
#   Spośród aut marki Toyota, który model ma największy medianowy przebieg?
auta2012 %>% 
  filter(Marka == "Toyota") %>%
  group_by(Model) %>% 
  summarise(n = n(), med_przeb=median(Przebieg.w.km)) %>% 
  arrange(desc(med_przeb)) %>%
  head(3)
#   Spośród aut z silnikiem diesla wyprodukowanych w 2007 roku który model jest najdroższy?
auta2012 %>% 
  filter(Rodzaj.paliwa == "olej napedowy (diesel)", Rok.produkcji == 2007) %>%
  group_by(Marka, Model) %>%
  summarise(srCena = mean(Cena.w.PLN)) %>%
  head(3)
#   W jakim modelu klimatyzacja jest najczęściej obecna?
auta2012 %>% 
  mutate(ma_klime = grepl("klimatyzacja", as.character(Wyposazenie.dodatkowe))) %>%
  group_by(Marka, Model, ma_klime) %>% 
  filter(ma_klime == TRUE) %>%
  summarise(n = n(), mean_klima = mean(ma_klime)) %>% 
  arrange(desc(mean_klima)) %>%
  head(100)
#   Gdy ograniczyć się tylko do aut o przebiegu poniżej 50 000 km o silniku diesla, która Marka występuje najczęściej w zbiorze danych auta2012?
auta2012 %>% 
  filter(Przebieg.w.km > 50000, Rodzaj.paliwa == "olej napedowy (diesel)") %>%
  group_by(Marka) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>%
  head(3)
#   Spośród aut marki Toyota wyprodukowanych w 2007 roku, który model jest średnio najdroższy?
auta2012 %>% 
  filter(Marka == "Toyota") %>%
  group_by(Model) %>% 
  summarise(n = n(), srCena=mean(Cena.w.PLN)) %>% 
  arrange(desc(srCena)) %>%
  head(3)
#   Spośród aut z silnikiem diesla wyprodukowanych w 2007 roku który model jest najtańszy?
auta2012 %>% 
  filter(Rok.produkcji == 2007, Rodzaj.paliwa == "olej napedowy (diesel)") %>%
  group_by(Marka, Model) %>% 
  summarise(n = n(), srCena=mean(Cena.w.PLN)) %>% 
  arrange(srCena) %>%
  head(3)
#   Jakiego koloru auta mają największy medianowy przebieg?
auta2012 %>% 
  group_by(Kolor) %>%
  filter(!is.na(Przebieg.w.km)) %>%
  summarise(median_przeb = median(Przebieg.w.km)) %>% 
  arrange(desc(median_przeb)) %>%
  head(3)

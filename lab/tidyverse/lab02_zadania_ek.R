########################################### ZADANIA ######################################################

library(PogromcyDanych)
library(dplyr)
library(tidyr)

head(auta2012)

# 1. Która Marka występuje najczęściej w zbiorze danych auta2012?
auta2012 %>% group_by(Marka) %>% summarise(n=n()) %>% filter(n==max(n))

# 2. Spośród aut marki Toyota, który model występuje najczęściej.
auta2012 %>% filter(Marka=="Toyota") %>% group_by(Model) %>% summarise(n=n()) %>% filter(n==max(n))

# 3. Sprawdź ile jest aut z silnikiem diesla wyprodukowanych w 2007 roku?
auta2012 %>% filter(Rodzaj.paliwa=="olej napedowy (diesel)", Rok.produkcji == 2007) %>% nrow()

# 4. Jakiego koloru auta mają najmniejszy medianowy przebieg?
auta2012 %>% group_by(Kolor) %>% summarise(mediana = median(Przebieg.w.km, na.rm=TRUE)) %>%
  arrange(mediana) %>% head(1)

# 5. Gdy ograniczyć się tylko do aut wyprodukowanych w 2007, 
#    która Marka występuje najczęściej w zbiorze danych auta2012?
auta2012 %>% filter(Rok.produkcji == 2007) %>% group_by(Marka) %>% summarise(n=n()) %>% filter(n==max(n))

# 6. Spośród aut marki Toyota, który model najbardziej stracił na cenie pomiędzy rokiem produkcji 2007 a 2008.
auta2012 %>% filter(Marka == "Toyota", Rok.produkcji %in% c(2007,2008)) %>% group_by(Model,Rok.produkcji) %>% 
  summarise(sr_cena = mean(Cena.w.PLN, na.rm=TRUE)) %>% spread(Rok.produkcji, sr_cena) %>%
  mutate(roznica = `2008` - `2007`) %>% ungroup %>% filter(roznica == min(roznica, na.rm=TRUE))

# 7. Spośród aut z silnikiem diesla wyprodukowanych w 2007 roku która marka jest najdroższa?
auta2012 %>% filter(Rodzaj.paliwa=="olej napedowy (diesel)", Rok.produkcji == 2007) %>%
  group_by(Marka) %>% summarise(sr_cena = mean(Cena.w.PLN, na.rm=TRUE)) %>% filter(sr_cena==max(sr_cena))

# 8. Ile jest aut z klimatyzacją?
auta2012 %>% select(Wyposazenie.dodatkowe) %>% filter(grepl("klimatyzacja", Wyposazenie.dodatkowe)) %>% nrow()

# 9. Gdy ograniczyć się tylko do aut z silnikiem ponad 100 KM, 
#    która Marka występuje najczęściej w zbiorze danych auta2012?
auta2012 %>% filter(KM > 100) %>% group_by(Marka) %>% summarise(n=n()) %>% filter(n==max(n))

# 10. Spośród aut marki Toyota, który model ma największą różnicę cen gdy porównać silniki benzynowe a diesel?
auta2012 %>% filter(Marka == "Toyota", Rodzaj.paliwa %in% c("benzyna", "olej napedowy (diesel)")) %>% group_by(Model,Rodzaj.paliwa) %>% 
  summarise(sr_cena = mean(Cena.w.PLN, na.rm=TRUE)) %>% spread(Rodzaj.paliwa, sr_cena) %>%
  mutate(roznica = benzyna - `olej napedowy (diesel)`) %>% ungroup %>% filter(roznica == max(abs(roznica), na.rm=TRUE))

# 11. Spośród aut z silnikiem diesla wyprodukowanych w 2007 roku która marka jest najtańsza?
auta2012 %>% filter(Rodzaj.paliwa=="olej napedowy (diesel)", Rok.produkcji == 2007) %>%
  group_by(Marka) %>% summarise(sr_cena = mean(Cena.w.PLN, na.rm=TRUE)) %>% filter(sr_cena==min(sr_cena))

# 12. W jakiej marce klimatyzacja jest najczęściej obecna?
auta2012 %>% filter(grepl("klimatyzacja", Wyposazenie.dodatkowe)) %>% group_by(Marka) %>% summarise(n = n()) %>% filter(n == max(n))

# 13. Gdy ograniczyć się tylko do aut o cenie ponad 50 000 PLN, która Marka występuje najczęściej w zbiorze danych auta2012?
auta2012 %>% filter(Cena.w.PLN>50000) %>% group_by(Marka) %>% summarise(n=n()) %>% filter(n==max(n))

# 14. Spośród aut marki Toyota, który model ma największy medianowy przebieg?
auta2012 %>% 
  filter(Marka == "Toyota") %>%
  group_by(Model) %>% summarise(mediana = median(Przebieg.w.km, na.rm=TRUE)) %>%
  arrange(desc(mediana)) %>% head(1)

# 15. Spośród aut z silnikiem diesla wyprodukowanych w 2007 roku który model jest najdroższy?
auta2012 %>% 
  filter(Rodzaj.paliwa=="olej napedowy (diesel)", Rok.produkcji == 2007) %>%
  group_by(Model) %>% summarise(sr_cena = mean(Cena.w.PLN, na.rm=TRUE)) %>% filter(sr_cena==max(sr_cena))

# 16. W jakim modelu klimatyzacja jest najczęściej obecna?
auta2012 %>% filter(grepl("klimatyzacja", Wyposazenie.dodatkowe)) %>% group_by(Model) %>% summarise(n = n()) %>% filter(n == max(n))

# 17. Gdy ograniczyć się tylko do aut o przebiegu poniżej 50 000 km o silniku diesla, 
#     która Marka występuje najczęściej w zbiorze danych auta2012?
auta2012 %>% filter(Przebieg.w.km<50000, Rodzaj.paliwa=="olej napedowy (diesel)") %>% 
  group_by(Marka) %>% summarise(n=n()) %>% filter(n==max(n))

# 18. Spośród aut marki Toyota wyprodukowanych w 2007 roku, który model jest średnio najdroższy?
auta2012 %>% filter(Marka=="Toyota", Rok.produkcji==2007) %>% group_by(Model) %>% summarise(sr_cena = mean(Cena.w.PLN, na.rm=TRUE)) %>%
  ungroup %>% filter(sr_cena==max(sr_cena))

# 19. Spośród aut z silnikiem diesla wyprodukowanych w 2007 roku który model jest najtańszy?
auta2012 %>% filter(Rok.produkcji==2007, Rodzaj.paliwa=="olej napedowy (diesel)") %>%
  group_by(Model) %>% summarise(sr_cena = mean(Cena.w.PLN, na.rm=TRUE)) %>% filter(sr_cena==min(sr_cena))

# 20. Jakiego koloru auta mają największy medianowy przebieg?
auta2012 %>% group_by(Kolor) %>% summarise(mediana = median(Przebieg.w.km, na.rm=TRUE)) %>%
  arrange(desc(mediana)) %>% head(1)

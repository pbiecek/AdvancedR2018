
#ZADANIA

#1. Która Marka wystêpuje najczêœciej w zbiorze danych auta2012?

auta %>%
  group_by(Marka) %>%
  count() %>%
  arrange(desc(n)) %>%
  head(1)

# 2. Spoœród aut marki Toyota, który model wystêpuje najczêœciej.


auta %>%
  filter(Marka == "Toyota") %>%
  group_by(Model) %>%
  summarise( n = n()) %>%
  arrange( desc(n)) %>%
  head(1)

# 3. SprawdŸ ile jest aut z silnikiem diesla wyprodukowanych w 2007 roku?

auta %>%
  filter(Rodzaj.paliwa == "olej napedowy (diesel)", Rok.produkcji == 2007) %>%
  count()

# 4. Jakiego koloru auta maj¹ najmniejszy medianowy przebieg?


auta2012 %>%
  group_by(Kolor) %>%
  summarise( przeb.med = median(Przebieg.w.km, na.rm = TRUE) ) %>%
  arrange(przeb.med) %>%
  head(1)

# 5. Gdy ograniczyæ siê tylko do aut wyprodukowanych w 2007, która Marka wystêpuje najczêœciej w zbiorze danych auta2012?

auta2012 %>%
  filter(Rok.produkcji == 2007) %>%
  group_by( Marka) %>%
  count() %>%
  arrange(desc(n)) %>%
  head(1)

# 6. Spoœród aut marki Toyota, który model najbardziej straci³ na cenie pomiêdzy rokiem produkcji 2007 a 2008.

auta2012 %>%
  filter(Marka == "Toyota", Rok.produkcji %in% c(2007, 2008)) %>%
  select(Model,Cena.w.PLN.w.PLN, Rok.produkcji) %>%
  group_by(Model, Rok.produkcji) %>%
  summarise( src = mean(Cena.w.PLN.w.PLN)) %>%
  spread(., Rok.produkcji, src) %>%
  mutate( rozn = `2007` - `2008`) %>%
  arrange( desc(rozn)) %>%
  head(1)


# 7. Spoœród aut z silnikiem diesla wyprodukowanych w 2007 roku która marka jest najdro¿sza?


auta2012 %>%
  filter(Rodzaj.paliwa == "olej napedowy (diesel)", Rok.produkcji ==2007) %>%
  select(Marka, Cena.w.PLN) %>%
  group_by(Marka) %>%
  summarise( sr.Cena.w.PLN = mean(Cena.w.PLN)) %>%
  arrange(sr.Cena.w.PLN) %>%
  head(1)

# 8. Ile jest aut z klimatyzacj¹?
head(auta2012)

auta2012 %>%
  mutate( czy.klim = grepl("klimatyzacja", Wyposazenie.dodatkowe, ignore.case = TRUE) ) %>%
  summarise( sum(czy.klim))

# 9. Gdy ograniczyæ siê tylko do aut z silnikiem ponad 100 KM, która Marka wystêpuje najczêœciej w zbiorze danych auta2012?

auta2012 %>%
  filter( KM > 100)  %>%
  group_by(Marka) %>%
  count() %>%
  arrange(desc(n)) %>%
  head(1)

# 10. Spoœród aut marki Toyota, który model ma najwiêksz¹ ró¿nicê cen gdy porównaæ silniki benzynowe a diesel?

# wroc
auta2012 %>%
  filter( Marka == "Toyota", Rodzaj.paliwa == "benzyna" | Rodzaj.paliwa ==  'olej napedowy (diesel)') %>%
  select(Model, Cena.w.PLN, Rodzaj.paliwa) %>%
  group_by( Model, Rodzaj.paliwa) %>%
  summarise( sr = mean(Cena.w.PLN)) %>%
  spread(., Rodzaj.paliwa, sr ) %>%
  mutate( rozn =  abs(benzyna - `olej napedowy (diesel)`)) %>%
  arrange(-rozn) %>%
  head(1)

# w pln 
auta2012 %>%
  filter( Marka == "Toyota", Rodzaj.paliwa %in% c( "benzyna" , 'olej napedowy (diesel)')) %>%
  select(Model, Cena.w.PLN.w.PLN, Rodzaj.paliwa) %>%
  group_by( Model, Rodzaj.paliwa) %>%
  summarise( sr = mean(Cena.w.PLN.w.PLN)) %>%
  spread(., Rodzaj.paliwa, sr ) %>%
  mutate( rozn =  abs(benzyna - `olej napedowy (diesel)`)) %>%
  arrange(-rozn) %>%
  head(1)


# 11. Spoœród aut z silnikiem diesla wyprodukowanych w 2007 roku która marka jest najtañsza?

auta2012 %>%
  filter( Rok.produkcji ==2007, Rodzaj.paliwa == "olej napedowy (diesel)" ) %>%
  group_by( Marka) %>%
  summarise( sr.cen = mean(Cena.w.PLN)) %>%
  arrange(desc(sr.cen)) %>%
  head(1)

# 12. W jakiej marce klimatyzacja jest najczêœciej obecna?

auta2012 %>%
  mutate( czy.klim = grepl("klimatyzacja", Wyposazenie.dodatkowe, ignore.case = TRUE) ) %>%
  filter( czy.klim == 1) %>%
  group_by(Marka) %>%
  count() %>%
  arrange( desc(n)) %>%
  head(1)

# 13. Gdy ograniczyæ siê tylko do aut o cenie ponad 50 000 PLN, która Marka wystêpuje najczêœciej w zbiorze danych auta2012?

auta2012 %>%
  filter(Cena.w.PLN.w.PLN >50000)%>%
  group_by(Marka) %>%
  count() %>%
  arrange( desc(n)) %>%
  head(1)

# 14. Spoœród aut marki Toyota, który model ma najwiêkszy medianowy przebieg?

auta2012 %>%
  filter( Marka == "Toyota") %>%
  group_by( Model) %>%
  summarise( med = median(Przebieg.w.km, na.rm = TRUE)) %>%
  arrange( desc(med)) %>%
  head(1)

#  15. Spoœród aut z silnikiem diesla wyprodukowanych w 2007 roku który model jest najdro¿szy?


auta2012 %>%
  filter( Rodzaj.paliwa == "olej napedowy (diesel)", Rok.produkcji == 2007) %>%
  group_by(Model) %>%
  summarise( cen = mean(Cena.w.PLN)) %>%
  arrange( desc(cen)) %>%
  head(1)

# 16. W jakim modelu klimatyzacja jest najczêœciej obecna?

auta2012 %>%
  mutate( czy.klim = grepl("klimatyzacja", Wyposazenie.dodatkowe, ignore.case = TRUE) ) %>%
  filter( czy.klim == 1) %>%
  group_by(Model) %>%
  count() %>%
  arrange( desc(n)) %>%
  head(1)

# 17. Gdy ograniczyæ siê tylko do aut o przebiegu poni¿ej 50 000 km o silniku diesla, która Marka wystêpuje najczêœciej w zbiorze danych auta2012?

auta2012 %>%
  filter( Rodzaj.paliwa == "olej napedowy (diesel)", Przebieg.w.km < 50000) %>%
  group_by(Marka) %>%
  count() %>%
  arrange( desc(n)) %>%
  head(1)

# 18. Spoœród aut marki Toyota wyprodukowanych w 2007 roku, który model jest œrednio najdro¿szy?


auta2012 %>%
  filter( Marka == "Toyota", Rok.produkcji == 2007) %>%
  select(Model, Cena.w.PLN) %>%
  group_by(Model) %>%
  summarise( src = mean(Cena.w.PLN)) %>%
  arrange( desc(src)) %>%
  head(1)

# 19. Spoœród aut z silnikiem diesla wyprodukowanych w 2007 roku który model jest najtañszy?

auta2012 %>%
  filter( Rodzaj.paliwa == "olej napedowy (diesel)", Rok.produkcji == 2007) %>% 
  group_by(Model) %>%
  summarise(sr = mean(Cena.w.PLN))%>%
  arrange( sr) %>%
  head(1)

# 20.  Jakiego koloru auta maj¹ najwiêkszy medianowy przebieg?

auta2012 %>%
  group_by(Kolor) %>%
  summarise( przeb.med = median(Przebieg.w.km, na.rm = TRUE) ) %>%
  arrange(desc(przeb.med)) %>%
  head(1)






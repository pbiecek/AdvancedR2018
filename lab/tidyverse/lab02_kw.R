##################
#zadania
# 
head(auta2012)

auta <- auta2012[,c("Cena.w.PLN", "KM", "Marka", "Model", "Przebieg.w.km",
                    "Rodzaj.paliwa", "Rok.produkcji","Kolor")]
head(auta[100:110,])
unique(auta$Rodzaj.paliwa)
#1 Która Marka wystêpuje najczêœciej w zbiorze danych auta2012?
auta %>% 
  group_by(Marka) %>% 
  summarise(licznosc = n()) %>% 
  arrange(-licznosc) %>% 
  head(1)
#Volkswagen
#2 Spoœród aut marki Toyota, który model wystêpuje najczêœciej.
auta %>% 
  filter(Marka == "Toyota") %>% 
  group_by(Model) %>% 
  summarise(licznosc = n()) %>% 
  arrange(-licznosc) %>% 
  head(1)
#Yaris     1552

#3 SprawdŸ ile jest aut z silnikiem diesla wyprodukowanych w 2007 roku?
auta %>% 
  filter(stri_detect_fixed(Rodzaj.paliwa,  "diesel"), Rok.produkcji==2007) %>% 
  summarise(licznosc = n())

#4 Jakiego koloru auta maj¹ najmniejszy medianowy przebieg?
auta %>% 
  filter(!is.na(Kolor), Kolor != '') %>% 
  group_by(Kolor) %>% 
  summarise(med_przebieg = median(Przebieg.w.km, na.rm=TRUE)) %>% 
  arrange(med_przebieg) %>% 
  head(1)

# bialy-metallic

#5 Gdy ograniczyæ siê tylko do aut wyprodukowanych w 2007, która Marka wystêpuje najczêœciej w zbiorze danych auta2012?
auta %>% 
  filter( Rok.produkcji==2007) %>% 
  group_by(Marka) %>% 
  summarise(licznosc = n()) %>% 
  arrange(-licznosc) %>% 
  head()



#6 Spoœród aut marki Toyota, który model najbardziej straci³ na cenie pomiêdzy rokiem produkcji 2007 a 2008.
auta %>% 
  filter( Marka=="Toyota",Rok.produkcji %in% c(2007,2008)) %>% 
  group_by(Model, Rok.produkcji) %>% 
  summarise(sr_Cena = mean(Cena.w.PLN)) %>% 
  spread(Rok.produkcji,sr_Cena) %>% 
  mutate(diff_price=`2008` - `2007`) %>% 
  filter(!is.na(diff_price)) %>% 
  arrange(diff_price) %>% 
  head(1)
#Hiace  49900  19900     -30000


#7 Spoœród aut z silnikiem diesla wyprodukowanych w 2007 roku która marka jest najdro¿sza?
auta2012 %>% 
  filter(stri_detect_fixed(Rodzaj.paliwa,  "diesel"), Rok.produkcji==2007) %>% 
  group_by(Marka) %>% 
  summarise(sr_Cena = mean(Cena.w.PLN, na.rm=TRUE)) %>% 
  arrange(-sr_Cena) %>% 
  head(1)

#8 Ile jest aut z klimatyzacj¹?
auta2012 %>% 
  filter(stri_detect_fixed(Wyposazenie.dodatkowe,  "klimatyzacja")) %>% 
  summarise(licznosc = n()) 

# 162960

#9 Gdy ograniczyæ siê tylko do aut z silnikiem ponad 100 KM, która Marka wystêpuje najczêœciej w zbiorze danych auta2012?
auta %>% 
  filter(KM > 100) %>% 
  group_by(Marka) %>% 
  summarise(licznosc = n()) %>% 
  arrange(-licznosc) %>% 
  head(1)
#Volkswagen    13317

#10 Spoœród aut marki Toyota, który model ma najwiêksz¹ ró¿nicê cen gdy porównaæ silniki benzynowe a diesel?
auta %>% 
  filter( Marka=="Toyota") %>% 
  mutate(silnik = ifelse(stri_detect_fixed(Rodzaj.paliwa,  "diesel"), "diesel", ifelse(stri_detect_fixed(Rodzaj.paliwa,  "benzyna"),"benzyna", "inny"))) %>% 
  filter(silnik %in% c("benzyna", "diesel")) %>% 
  group_by(Model, silnik) %>% 
  summarise(sr_Cena = mean(Cena.w.PLN)) %>% 
  spread(silnik,sr_Cena) %>% 
  mutate(diff_price=abs(benzyna-diesel) ) %>% 
  filter(!is.na(diff_price)) %>% 
  arrange(-diff_price) %>% 
  head(1)

#Camry   43289   3200      40089

#11 Spoœród aut z silnikiem diesla wyprodukowanych w 2007 roku która marka jest najtañsza?
auta %>% 
  filter(stri_detect_fixed(Rodzaj.paliwa,  "diesel"), Rok.produkcji==2007) %>% 
  group_by(Marka) %>% 
  summarise(sr_Cena = mean(Cena.w.PLN)) %>% 
  arrange(sr_Cena) %>% 
  head(1)

#Aixam   13533

#12 W jakiej marce klimatyzacja jest najczêœciej obecna?
auta2012 %>% 
  mutate(czy_klimatyzacja = ifelse(stri_detect_fixed(Wyposazenie.dodatkowe,  "klimatyzacja"),1,0)) %>% 
  group_by(Marka) %>% 
  summarise(ile_klimatyzacja = sum(czy_klimatyzacja)) %>% 
  #mutate(freq_klimatyzacja=ile_klimatyzacja/licznosc) %>% 
  arrange(-ile_klimatyzacja ) %>% 
  head(1)


#13 Gdy ograniczyæ siê tylko do aut o cenie ponad 50 000 PLN, która Marka wystêpuje najczêœciej w zbiorze danych auta2012?
auta2012 %>% 
  filter(Cena.w.PLN>50000) %>% 
  group_by(Marka) %>% 
  summarise(licznosc = n()) %>% 
  arrange(-licznosc) %>% 
  head(1)

#Audi      4374

#14 Spoœród aut marki Toyota, który model ma najwiêkszy medianowy przebieg?

auta %>% 
  filter(Marka == "Toyota") %>% 
  group_by(Model) %>% 
  summarise(med = median(Przebieg.w.km, na.rm=TRUE)) %>% 
  arrange(-med) %>% 
  head(1)

#Carina 203000

#15 Spoœród aut z silnikiem diesla wyprodukowanych w 2007 roku który model jest najdro¿szy?
auta %>% 
  filter(stri_detect_fixed(Rodzaj.paliwa,  "diesel"), Rok.produkcji==2007) %>% 
  group_by(Marka,Model) %>% 
  summarise(sr_Cena = mean(Cena.w.PLN)) %>% 
  arrange(-sr_Cena) %>% 
  head(1)


#16 W jakim modelu klimatyzacja jest najczêœciej obecna?
auta2012 %>% 
  mutate(czy_klimatyzacja = ifelse(stri_detect_fixed(Wyposazenie.dodatkowe,  "klimatyzacja"),1,0)) %>% 
  group_by(Model) %>% 
  summarise(ile_klimatyzacja = sum(czy_klimatyzacja), licznosc = n()) %>% 
  mutate(freq_klimatyzacja=ile_klimatyzacja/licznosc) %>% 
  arrange(-freq_klimatyzacja ) %>% 
  head()


#17 Gdy ograniczyæ siê tylko do aut o przebiegu poni¿ej 50 000 km o silniku diesla, która Marka wystêpuje najczêœciej w zbiorze danych auta2012?
auta %>% 
  filter(Przebieg.w.km < 50000, stri_detect_fixed(Rodzaj.paliwa,  "diesel")) %>% 
  group_by(Marka) %>% 
  summarise(licznosc = n()) %>% 
  arrange(-licznosc) %>% 
  head(1)

#BMW       1217


#18 Spoœród aut marki Toyota wyprodukowanych w 2007 roku, który model jest œrednio najdro¿szy?

auta %>% 
  filter(Marka == "Toyota", Rok.produkcji==2007) %>% 
  group_by(Model) %>% 
  summarise(sr_cena = mean(Cena.w.PLN)) %>% 
  arrange(-sr_cena) %>% 
  head(1)
#Land Cruiser  101566


#19 Spoœród aut z silnikiem diesla wyprodukowanych w 2007 roku który model jest najtañszy?

auta %>% 
  filter(stri_detect_fixed(Rodzaj.paliwa,  "diesel"), Rok.produkcji==2007) %>% 
  group_by(Marka,Model) %>% 
  summarise(sr_cena = mean(Cena.w.PLN)) %>% 
  arrange(sr_cena) %>% 
  head(1)
# Lancia Thesis    2000


#20 Jakiego koloru auta maj¹ najwiêkszy medianowy przebieg?
# 

auta %>% 
  filter(!is.na(Kolor), Kolor != '') %>% 
  group_by(Kolor) %>% 
  summarise(med_przebieg = median(Przebieg.w.km, na.rm=TRUE)) %>% 
  arrange(-med_przebieg) %>% 
  head(1)

# bordowy       175500


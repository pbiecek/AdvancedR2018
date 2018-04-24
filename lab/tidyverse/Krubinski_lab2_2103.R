
library('PogromcyDanych')
library(dplyr)

auta <- auta2012[,c("Cena.w.PLN",'KM','Marka','Model','Przebieg.w.km','Rodzaj.paliwa','Rok.produkcji')]
head(auta,2)
auta[auta$Cena == (max(auta$Cena.w.PLN)),]


# z dplyr
tanieAuta <- filter(auta,Cena.w.PLN < 50000)
# auta[auta$Cena.w.PLN < 5000,]

tanieAuta <- filter(auta,Cena.w.PLN < 1000, Marka == 'Fiat')
# & $ == ,

# logicznych z R nie umie do sql, a przecinki i in sobie przelozy na kłery

# filter - wybor wierszy


# select - wybor kolumn
a <- select(auta, 1:2)

# sortowanie
tmp <- auta[order(auta$Cena.w.PLN),]
tmp <- arrange(tanieAuta, Rodzaj.paliwa,desc(Cena.w.PLN))

tm <- filter(auta,Marka == 'Nissan')
head(tm)

`%ble%` <- function(x,y) paste(x,'',y)ble
'hey,' %ble% 'you'




auta %>%
  filter(Cena.w.PLN < 5000) %>%
  select(-Rok.produkcji) %>%
  arrange(Rodzaj.paliwa, desc(Cena.w.PLN)) %>%
  head(5) -> noweauta

noweauta

auta %>%
  lm(Cena.w.PLN~Rok.produkcji, data=.)

c('ble', 'Ala','G') %>% paste(.,.)


# # summarize i griup by


auta %>%
  summarise(srCena = mean(Cena.w.PLN),
            n  = n(), # dzial jak length/count, ale umei do sql przekazac
            mPrzebieg = median(Przebieg.w.km, na.rm = T))


auta %>% 
  group_by(Marka,Rodzaj.paliwa) %>%
  summarise(srCena = mean(Cena.w.PLN),
            n  = n(), # dzial jak length/count, ale umei do sql przekazac
            mPrzebieg = median(Przebieg.w.km, na.rm = T)) %>%
  filter(n > 20) %>% arrange(-srCena)
# data frame obecenie uważane za wolne, stosuje sie inne struktury - tibble

# mutate - dodaje nwoa kolumne

# UWAGA BO SREDNAI PO GROUP BY 

auta %>% 
  group_by(Marka) %>%
  mutate(min = min(Cena.w.PLN)) %>%
  mutate(nowa = Cena.w.PLN/mean(Cena.w.PLN))  %>% 
  ungroup() %>%
  mutate(min1 = min(Cena.w.PLN)) -> newA

head(newA,4)



auta %>%
  filter(Rok.produkcji > 2005) %>%
  group_by(Marka, Rok.produkcji,Przebieg.w.km) %>%
  summarise(srCena = mean(Cena.w.PLN)) -> nA


library(tidyr)

# 2 funkcje, maly pakiet
head(nA)
spread(nA,Rok.produkcji,srCena) -> szernA
head(szernA)
spread(nA,Marka,srCena) -> markanA
head(markanA)


markanA %>% gather(Marka,srednia_cena,-Przebieg.w.km,-Rok.produkcji) -> nq

head(nq)


eur <- eurostat::get_eurostat('gov_10a_main')


#ZADANIA 


auta <- PogromcyDanych::auta2012  

#Zad1
auta %>% 
  group_by(Marka) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%head(1)

#Zad2
View(colnames(auta))
auta %>%
  filter(Marka == 'Toyota')%>%
  group_by(Model) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%head(1)

#ZAd3
auta %>%
  filter(Rodzaj.paliwa == 'olej napedowy (diesel)',Rok.produkcji == '2007') %>%
  summarise(n = n())

#Zad 4
auta %>%
  group_by(Kolor) %>%
  summarize(n = median(Przebieg.w.km,na.rm=T))%>%
  arrange(n) %>% head(1) 

#Zad 5
auta %>%
  filter(Rok.produkcji == '2007') %>%
  group_by(Marka) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>% head(1)

#Zad 6

#Spośród aut marki Toyota, który model najbardziej stracił na cenie pomiędzy rokiem produkcji 2007 a 2008

auta %>% 
  filter(Marka == 'Toyota',Rok.produkcji %in% c('2007','2008')) %>%
  group_by(Model,Rok.produkcji) %>%
  summarise(sr = mean(Cena.w.PLN,na.rm=T)) %>%
  spread(Rok.produkcji,sr) %>%
  mutate(diff = `2007` - `2008`) %>%
  arrange(diff) %>%  head(1)
  
#Zad 7
#Spośród aut z silnikiem diesla wyprodukowanych w 2007 roku która marka jest najdroższa?

auta %>% 
  filter(Rodzaj.paliwa == 'olej napedowy (diesel)',Rok.produkcji == '2007') %>%
  group_by(Marka) %>%
  summarise(n = mean(Cena.w.PLN)) %>%
  arrange(desc(n)) %>% head(1)

#Zad 8
#Ile jest aut z klimatyzacją?

auta %>%
  filter(grepl('klimatyzacja',Wyposazenie.dodatkowe)) %>%
  summarise(n = n()) %>% head(1)
#Zad 9
#Gdy ograniczyć się tylko do aut z silnikiem ponad 100 KM, która Marka występuje najczęściej w zbiorze danych auta2012

auta %>%
  filter(KM > 100) %>%
  group_by(Marka) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>% head(1)

#Zad 10
#Spośród aut marki Toyota, który model ma największą różnicę cen gdy porównać silniki benzynowe a diesel?

auta %>%
  filter(Marka == 'Toyota', Rodzaj.paliwa %in% c('olej napedowy (diesel)','benzyna')) %>%
  group_by(Model,Rodzaj.paliwa) %>%
  summarise(sr = mean(Cena.w.PLN)) %>%
  spread(Rodzaj.paliwa,sr) %>%
  mutate(diff = benzyna - `olej napedowy (diesel)`) %>%
  arrange(desc(diff)) %>% head(1)

# Zad 11
#pośród aut z silnikiem diesla wyprodukowanych w 2007 roku która marka jest najtańsza?

auta %>%
  filter(Rodzaj.paliwa == 'olej napedowy (diesel)',Rok.produkcji == '2007') %>%
  group_by(Marka) %>%
  summarise(sr = mean(Cena.w.PLN)) %>%
  arrange(sr) %>% head(1)

#Zad 12
#W jakiej marce klimatyzacja jest najczęściej obecna?
auta %>%
  group_by(Marka) %>%
  mutate(con = n()) %>%
  ungroup() %>% 
  filter(grepl('klimatyzacja',Wyposazenie.dodatkowe)) %>%
  group_by(Marka) %>%
  summarise(sr = n()/mean(con)) %>%
  arrange(desc(sr)) %>% head(1)
#Zad 13
#Gdy ograniczyć się tylko do aut o cenie ponad 50 000 PLN, która Marka występuje najczęściej w zbiorze danych auta2012?

auta %>% 
  filter(Cena.w.PLN > 50000) %>%
  group_by(Marka) %>%
  summarise(sr = n()) %>%
  arrange(desc(sr)) %>% head(1)

#Zad 14
#Spośród aut marki Toyota, który model ma największy medianowy przebieg?

auta %>%
  filter(Marka == 'Toyota') %>%
  group_by(Model) %>%
  summarise(sr = median(Przebieg.w.km,na.rm=T)) %>%
  arrange(desc(sr)) %>% head(1)

#Zad 15
#Spośród aut z silnikiem diesla wyprodukowanych w 2007 roku który model jest najdroższy?

auta %>%
  filter(Rodzaj.paliwa == 'olej napedowy (diesel)',Rok.produkcji == '2007') %>%
  group_by(Model) %>%
  summarise(sr = mean(Cena.w.PLN)) %>%
  arrange(desc(sr)) %>% head(1)

# Zad 16
#W jakim modelu klimatyzacja jest najczęściej obecna?

auta %>%
  group_by(Model) %>%
  mutate(con = n()) %>%
  ungroup() %>%
  filter(grepl('klimatyzacja',Wyposazenie.dodatkowe)) %>%
  group_by(Model) %>%
  summarise(sr = n()/mean(con)) %>%
  arrange(desc(sr)) %>% head(1)

# Zad 17
#Gdy ograniczyć się tylko do aut o przebiegu poniżej 50 000 km o silniku diesla, która Marka występuje najczęściej w zbiorze danych auta2012?
auta %>%
  filter(Przebieg.w.km < 50000,Rodzaj.paliwa == 'olej napedowy (diesel)') %>% 
  group_by(Marka) %>%
  summarise(sr = n()) %>%
  arrange(desc(sr)) %>% head(1)

#Zad 18
#Spośród aut marki Toyota wyprodukowanych w 2007 roku, który model jest średnio najdroższy?

auta %>%
  filter(Marka == 'Toyota', Rok.produkcji == '2007') %>%
  group_by(Model) %>%
  summarise(sr = mean(Cena.w.PLN)) %>%
  arrange(desc(sr)) %>% head(1)

# Zad 19
# Spośród aut z silnikiem diesla wyprodukowanych w 2007 roku który model jest najtańszy?

auta %>%
  filter(Rodzaj.paliwa == 'olej napedowy (diesel)', Rok.produkcji == '2007') %>%
  group_by(Model) %>%
  summarise(sr = mean(Cena.w.PLN)) %>%
  arrange(sr) %>% head(1)

#Zad 20
#Jakiego koloru auta mają największy medianowy przebieg?

auta %>%
  group_by(Kolor) %>%
  summarise(sr = median(Przebieg.w.km,na.rm=T)) %>%
  arrange(desc(sr)) %>% head(1)

          
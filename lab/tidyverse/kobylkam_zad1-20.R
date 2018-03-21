#ZADANIA

library(PogromcyDanych)
library(dplyr)
library(tidyr)

#1

auta2012 %>% group_by(Marka) %>% summarize(n = n()) %>% filter(n == max(n)) %>% select(Marka)

#2

auta2012 %>% group_by(Marka, Model) %>% summarize(n = n()) %>% filter (Marka=="Toyota", n==max(n)) %>% select(Marka,Model)

#3

auta2012 %>% filter(Rodzaj.paliwa=="olej napedowy (diesel)",Rok.produkcji==2007) %>%
        group_by(Rodzaj.paliwa,Rok.produkcji) %>%
        summarize(n=n()) 
#4

auta2012 %>% group_by(Kolor) %>% summarize(mediana=median(Przebieg.w.km, na.rm=T)) %>% filter(mediana==min(mediana))

#5

auta2012 %>% filter(Rok.produkcji == 2007) %>% group_by(Marka) %>% summarize(n=n()) %>% filter(n==max(n))

#6

auta2012 %>% filter(Marka=="Toyota",Rok.produkcji %in% c(2007,2008)) %>% group_by(Model,Rok.produkcji) %>%
        summarize(srCena = mean(Cena.w.PLN)) %>% spread(.,Rok.produkcji,srCena) %>%
        mutate(roznica = `2008`-`2007`) %>% arrange(roznica) %>% head(1)

#7
  
auta2012 %>% filter(Rodzaj.paliwa=="olej napedowy (diesel)",Rok.produkcji == 2007) %>% group_by(Marka) %>% 
            summarize(srCena=mean(Cena.w.PLN)) %>% filter(srCena == max(srCena))

#8

auta2012 %>% mutate(czy.klimatyzacja = grepl("klimatyzacja",Wyposazenie.dodatkowe)) %>%
        summarize(ilosc=sum(czy.klimatyzacja))

#9

auta2012 %>% filter(KM > 100) %>% group_by(Marka) %>% summarize(n=n()) %>% filter(n==max(n))

#10


auta2012 %>% filter(Marka=="Toyota",Rodzaj.paliwa %in% c("benzyna", "olej napedowy (diesel)")) %>%
  group_by(Model,Rodzaj.paliwa) %>% summarize(srCena = mean(Cena.w.PLN)) %>% 
  spread(.,Rodzaj.paliwa,srCena) %>% mutate(roznica = abs(benzyna - `olej napedowy (diesel)`)) %>%
  arrange(-roznica) %>% head(1)

#11

auta2012 %>% filter(Rodzaj.paliwa=="olej napedowy (diesel)",Rok.produkcji == 2007) %>% group_by(Marka) %>% 
  summarize(srCena=mean(Cena.w.PLN)) %>% filter(srCena == min(srCena))

#12

auta2012 %>% mutate(czy.klimatyzacja = grepl("klimatyzacja",Wyposazenie.dodatkowe)) %>% 
        group_by(Marka) %>% summarize(ilosc = sum(czy.klimatyzacja)) %>% filter(ilosc == max(ilosc))

#13

auta2012 %>% filter(Cena.w.PLN > 50000) %>% group_by(Marka) %>% summarize(n=n()) %>% filter(n == max(n))

#14

auta2012 %>% filter(Marka == "Toyota") %>% group_by(Model) %>% 
        summarize(med.przebieg = median(Przebieg.w.km,na.rm=T)) %>% 
        arrange(med.przebieg) %>% head(1)

#15

auta2012 %>% filter(Rodzaj.paliwa=="olej napedowy (diesel)",Rok.produkcji == 2007) %>% group_by(Model) %>%
          summarize(srCena = mean(Cena.w.PLN)) %>%  arrange(-srCena) %>% head(1)


#16

auta2012 %>% mutate(czy.klimatyzacja = grepl("klimatyzacja",Wyposazenie.dodatkowe)) %>% 
  group_by(Model) %>% summarize(ilosc = sum(czy.klimatyzacja)) %>% filter(ilosc == max(ilosc))

#17

auta2012 %>% filter(Przebieg.w.km < 50000, Rodzaj.paliwa=="olej napedowy (diesel)") %>%
          group_by(Marka) %>% summarize(n=n()) %>% filter(n == max(n))

#18

auta2012 %>% filter(Marka == "Toyota", Rok.produkcji==2007) %>% group_by(Model) %>% 
  summarize(srCena = mean(Cena.w.PLN)) %>% arrange(-srCena) %>% head(1)

#19

auta2012 %>% filter(Rodzaj.paliwa=="olej napedowy (diesel)",Rok.produkcji == 2007) %>% group_by(Model) %>%
  summarize(srCena = mean(Cena.w.PLN)) %>%  arrange(srCena) %>% head(1)

#20

auta2012 %>% group_by(Kolor) %>% summarize(mediana=median(Przebieg.w.km, na.rm=T)) %>% filter(mediana==max(mediana))



library(PogromcyDanych)
library(dplyr)
#ZADANIA


#1. Która Marka występuje najczęściej w zbiorze danych auta2012
auta %>% 
  select(Marka) %>% 
  group_by(Marka) %>% count %>% arrange(desc(n)) %>% head(1)

#2.  Spośród aut marki Toyota, który model występuje najczęściej.
auta %>% 
  filter(Marka=="Toyota") %>% 
  group_by(Model) %>% count %>% arrange(desc(n)) %>% head(1)

# 3.Sprawdź ile jest aut z silnikiem diesla wyprodukowanych w 2007 roku?  
auta %>% 
  filter(Rodzaj.paliwa=="olej napedowy (diesel)", Rok.produkcji==2007) %>% count 


# 4. Jakiego koloru auta mają najmniejszy medianowy przebieg?
names(auta2012)
auta2012 %>%
  select(Kolor,Przebieg.w.km) %>%
  group_by(Kolor)%>%
  summarize(mprzebieg=median(Przebieg.w.km,na.rm=TRUE)) %>% 
  arrange((mprzebieg))%>%
  head(1)
# bialy-metallic 

# 5.  Gdy ograniczyć się tylko do aut wyprodukowanych w 2007, która Marka występuje najczęściej w zbiorze danych auta2012?
auta2012%>% 
  filter(Rok.produkcji==2007)%>%
  group_by(Marka) %>% count %>%
  arrange(desc(n)) %>% head(1)
#Volkswagen 

# 6. Spośród aut marki Toyota, który model najbardziej stracił na cenie pomiędzy rokiem produkcji 2007 a 2008.
auta2012%>% 
  filter(Marka=="Toyota",Rok.produkcji %in%c(2007,2008)) %>%
  select(Model,Cena.w.PLN,Rok.produkcji)%>%
  group_by(Model, Rok.produkcji)%>%
  summarize(srcena=mean(Cena.w.PLN))%>%
  spread(.,Rok.produkcji, srcena)%>%
  mutate(spadek=`2007`-`2008`) %>% arrange(-spadek)%>%head(1)

# 7.Spośród aut z silnikiem diesla wyprodukowanych w 2007 roku która marka jest najdroższa?
auta%>%
  filter(Rodzaj.paliwa=="olej napedowy (diesel)", Rok.produkcji==2007)%>%
  group_by(Marka)%>% summarize(srednia=mean(Cena.w.PLN))%>% filter(srednia==max(srednia))


# 8. Ile jest aut z klimatyzacją?
names(auta2012)
auta2012 %>%
  mutate(klim=grepl("klimatyzacja", Wyposazenie.dodatkowe)) %>%
  summarize(ile=sum(klim))


# 9. Gdy ograniczyć się tylko do aut z silnikiem ponad 100 KM, 
# która Marka występuje najczęściej w zbiorze danych auta2012?
auta2012 %>%
  filter(KM>100)%>%
  group_by(Marka)%>%
  count%>%
  arrange(desc(n))%>% head(1)

# 10. Spośród aut marki Toyota, który model ma największą różnicę cen gdy porównać silniki benzynowe a diesel?
auta2012%>% 
  filter(Marka=="Toyota",Rodzaj.paliwa %in% c("olej napedowy (diesel)","benzyna")) %>%
  select(Model,Rodzaj.paliwa, Cena.w.PLN)%>%
  group_by(Model,Rodzaj.paliwa)%>%
  summarize(srcena=mean(Cena.w.PLN))%>%
  spread(.,Rodzaj.paliwa, srcena)%>%
  mutate(roznica=abs(`benzyna`-`olej napedowy (diesel)`)) %>% arrange(desc(roznica))%>%head(1)



#11.   Spośród aut z silnikiem diesla wyprodukowanych w 2007 roku która marka jest najtańsza?
auta%>%
  filter(Rodzaj.paliwa=="olej napedowy (diesel)", Rok.produkcji==2007)%>%
  group_by(Marka)%>% summarize(srednia=mean(Cena.w.PLN))%>% filter(srednia==min(srednia))

# 12. W jakiej marce klimatyzacja jest najczęściej obecna?

auta2012 %>%
  group_by(Marka) %>%
  mutate(klim=grepl("klimatyzacja", Wyposazenie.dodatkowe)) %>%
  summarize(ile=sum(klim)) %>% filter(ile==max(ile))

#13.Gdy ograniczyć się tylko do aut o cenie ponad 50 000 PLN, 
# która Marka występuje najczęściej w zbiorze danych auta2012?
auta2012 %>%
  filter(Cena.w.PLN>50000)%>%
  group_by(Marka)%>% count %>% arrange(desc(n)) %>% head(1)

#14.  Spośród aut marki Toyota, który model ma największy medianowy przebieg?

auta2012 %>%
  select(Marka,Model,Przebieg.w.km) %>%
  filter(Marka=="Toyota") %>%
  group_by(Model)%>%
  summarize(mprzebieg=median(Przebieg.w.km,na.rm=TRUE)) %>% 
  arrange(desc(mprzebieg))%>% head(1)


# 15.  Spośród aut z silnikiem diesla wyprodukowanych w 2007 roku który model jest najdroższy?
auta %>% 
  filter(Rodzaj.paliwa=="olej napedowy (diesel)", Rok.produkcji==2007) %>%
  group_by(Model)%>% summarize(srednia=mean(Cena.w.PLN))%>% filter(srednia==max(srednia))

# 16. W jakim modelu klimatyzacja jest najczęściej obecna?
auta2012 %>%
  group_by(Model) %>%
  mutate(klim=grepl("klimatyzacja", Wyposazenie.dodatkowe)) %>%
  summarize(ile=sum(klim)) %>% filter(ile==max(ile))

# 17. Gdy ograniczyć się tylko do aut o przebiegu poniżej 50 000 km o silniku diesla, 
# która Marka występuje najczęściej w zbiorze danych auta2012?

auta2012 %>%
  filter(Przebieg.w.km<50000, Rodzaj.paliwa=="olej napedowy (diesel)")%>%
  group_by(Marka)%>% count %>% arrange(desc(n)) %>% head(1)

# 18. Spośród aut marki Toyota wyprodukowanych w 2007 roku, który model jest średnio najdroższy?
auta%>% 
  filter(Marka=="Toyota",Rok.produkcji==2007) %>%
  group_by(Model) %>%
  summarize(srednia=mean(Cena.w.PLN))%>% filter(srednia==max(srednia))


# 19. Spośród aut z silnikiem diesla wyprodukowanych w 2007 roku który model jest najtańszy?
auta2012 %>%
  filter(Rodzaj.paliwa=="olej napedowy (diesel)", Rok.produkcji==2007) %>%
  group_by(Model)%>%
  summarize(srednia=mean(Cena.w.PLN))%>% filter(srednia==min(srednia))

#20. Jakiego koloru auta mają największy medianowy przebieg?

auta2012 %>%
  select(Kolor,Przebieg.w.km) %>%
  group_by(Kolor)%>%
  summarize(mprzebieg=median(Przebieg.w.km,na.rm=TRUE)) %>% 
  arrange(desc(mprzebieg))%>%
  head(1)

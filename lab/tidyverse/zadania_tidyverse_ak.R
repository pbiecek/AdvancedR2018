library(PogromcyDanych)
library(dplyr)

auta <- auta2012[,c("Cena.w.PLN","KM","Marka","Model","Przebieg.w.km","Rodzaj.paliwa","Rok.produkcji")]
auta[which.max(auta$Cena.w.PLN),]


auta[auta$Cena.w.PLN <50000 & auta$Marka=="Ford",]

tanieAuto <- filter(auta, Cena.w.PLN <50000, Marka == "Ford")


tanieAuto <- filter(auta, Cena.w.PLN <1000, Marka == c("Fiat"))
dim(tanieAuto)


tanieAuto <- select(auta, Marka:Przebieg.w.km,Rok.produkcji)
tanieAuto <- select(auta, -Rok.produkcji)

tmp <- arrange(tanieAuto, Rodzaj.paliwa, Cena.w.PLN)
tmp <- select(tmp, Marka,Rok.produkcji, Cena.w.PLN)
#dla zmiennych jakosciowych zamiast desc mozna uzyc "-"

noweAuta <- auta %>% filter(Cena.w.PLN < 50000) %>% select(-Rok.produkcji) %>% arrange(Rodzaj.paliwa, desc(Cena.w.PLN)) %>% head(2)

#operatory

'%kot%' <- function(x,y) paste0(x, " miau ",y)
'%kot%'("Ala", "puszek")
"Ala" %kot% "puszek"

#'%>%' <- function(x, f, ...) f(x, ...)



auta %>% filter(Cena.w.PLN < 50000) %>% select(-Rok.produkcji) %>% arrange(Rodzaj.paliwa, desc(Cena.w.PLN)) %>% head(2) -> noweAuta 


#model linowy
lm(Cena.w.PLN ~Rok.produkcji, data=auta)
auta %>% lm(Cena.w.PLN~Rok.produkcji,data=.)

"Ala" %>% paste(.,.)

auta %>% filter(Cena.w.PLN < 1000, Marka=="Fiat") %>% select(-Rok.produkcji) %>% arrange(Rodzaj.paliwa, desc(Cena.w.PLN))

auta %>% summarise(srCena=mean(Cena.w.PLN), n=n(), mPrzebieg = median(Przebieg.w.km,na.rm=TRUE))

auta %>%
  group_by(Marka, Rodzaj.paliwa) %>%
  summarise(srCena=mean(Cena.w.PLN),
            n=n(), 
            mPrzebieg = median(Przebieg.w.km,na.rm=TRUE)) %>%
  filter(n >20) %>%
  arrange(-srCena)
  

auta %>%
  group_by(Marka) %>%
  mutate(min = min(Cena.w.PLN),unormowana= Cena.w.PLN/mean(Cena.w.PLN))
# ungroup() %>% mutate()

auta %>% filter(Rok.produkcji> 2005) %>% group_by(Marka,Rok.produkcji) %>% summarise(srCena=mean(Cena.w.PLN)) ->agregat


library(tidyr)
spread(agregat,Rok.produkcji,srCena) -> szeroka
spread(agregat,Marka, srCena)

szeroka %>% gather(Rok, srednia_cena, -Marka)

library(eurostat)
df <- get_eurostat("nama_10_lp_ulc")

df1 <- spread(df, geo,values)
df1
tail(df1)



#Zadanka
#1
df <- auta2012
df %>% group_by(Marka) %>% summarise(n=n()) %>% arrange(-n)
#Volkswagen
#2
df %>% filter(Marka=="Toyota") %>% select(Model) %>% group_by(Model) %>% summarise(n=n()) %>% arrange(desc(n))
#Yaris        
#3
df %>% filter(Rok.produkcji==2007,Rodzaj.paliwa=="olej napedowy (diesel)") %>% nrow()
#11621
#4
df  %>% select(Kolor,Przebieg.w.km) %>% group_by(Kolor) %>% summarise(medianPrzebieg=median(Przebieg.w.km, na.rm=TRUE)) %>% arrange(medianPrzebieg)
#bialy-metallic 
#5
df %>% filter(Rok.produkcji==2007) %>% group_by(Marka) %>% summarise(n=n()) %>% arrange(-n)
#Volkswagen
#6
df %>% filter(Marka=="Toyota", Rok.produkcji %in% c(2007,2008)) %>% group_by(Rok.produkcji,Model) %>% select(Marka, Rok.produkcji, Model, Cena.w.PLN) %>% summarise(srednia=mean(Cena.w.PLN)) %>%
  spread(Rok.produkcji, srednia) %>% mutate(roznica = `2008`-`2007`) %>% arrange(roznica)
#Hiace 
#7
df %>% filter(Rodzaj.paliwa=="olej napedowy (diesel)",Rok.produkcji==2007) %>% group_by(Marka) %>% summarise(srednia=mean(Cena.w.PLN)) %>% arrange(-srednia)
#Porsche 
#8
df %>% filter(grepl("klimatyzacja",df$Wyposazenie.dodatkowe)==TRUE) %>% nrow()
#162960
#9
df %>% filter(KM >100) %>% group_by(Marka) %>% summarise(n=n()) %>% arrange(-n)
#Volkswagen
#10
df %>% filter(Marka=="Toyota", Rodzaj.paliwa %in% c("benzyna","olej napedowy (diesel)")) %>% group_by(Model,Rodzaj.paliwa) %>% select(Model,Rodzaj.paliwa, Cena.w.PLN) %>% summarise(srednia=mean(Cena.w.PLN)) %>%
  spread(Rodzaj.paliwa, srednia) %>% mutate(roznica = abs(benzyna - `olej napedowy (diesel)`)) %>% arrange(-roznica)
#Camry
#11
df %>% filter(Rodzaj.paliwa=="olej napedowy (diesel)", Rok.produkcji==2007) %>% group_by(Marka) %>% summarise(srednia = mean(Cena.w.PLN)) %>% arrange(srednia)
#Aixam
#12
df %>% filter(grepl("klimatyzacja",df$Wyposazenie.dodatkowe)==TRUE) %>% group_by(Marka) %>% summarise(n=n()) %>% arrange(-n)
#Volkswagen
#13
df %>% filter(Cena.w.PLN > 50000) %>% group_by(Marka) %>% summarise(n=n()) %>% arrange(-n)
#Audi  
#14
df %>% filter(Marka=="Toyota") %>% group_by(Model) %>% summarise(mediana = median(Przebieg.w.km, na.rm=TRUE)) %>% arrange(-mediana)
#Carina 
#15
df %>% filter(Rodzaj.paliwa=="olej napedowy (diesel)", Rok.produkcji==2007) %>% group_by(Model) %>% summarise(max= max(Cena.w.PLN)) %>% arrange(-max)
#G 320
#16
df %>% filter(grepl("klimatyzacja",df$Wyposazenie.dodatkowe)==TRUE) %>% group_by(Model) %>% summarise(n=n()) %>% arrange(-n)
#Passat
#17
df %>% filter(Przebieg.w.km < 50000,Rodzaj.paliwa=="olej napedowy (diesel)") %>% group_by(Marka) %>% summarise(n=n())%>% arrange(-n)
#BMW 
#18
df %>% filter(Marka=="Toyota",Rok.produkcji==2007) %>% group_by(Model) %>% summarise(srednia = mean(Cena.w.PLN, na.rm=TRUE)) %>% arrange(-srednia)
#Land Cruiser
#19
df %>% filter(Rodzaj.paliwa=="olej napedowy (diesel)", Rok.produkcji==2007) %>% group_by(Model) %>% summarise(max= max(Cena.w.PLN)) %>% arrange(max)
#Thesis
#20
df  %>% select(Kolor,Przebieg.w.km) %>% group_by(Kolor) %>% summarise(medianPrzebieg=median(Przebieg.w.km, na.rm=TRUE)) %>% arrange(-medianPrzebieg)
#bordowy 
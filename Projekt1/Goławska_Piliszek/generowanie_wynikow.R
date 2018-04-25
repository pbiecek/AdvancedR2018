#rysowanie chmur punktow

#korzystamy z plikow .csv wygenerowanych w nastepujacy sposb:
write.csv(bib_i_czest_dla_wszystkich_skryptowR(x)[[1]], "pakiety_w_skryptach.csv")
write.csv(wszystkie_descr(x)[[1]], "pakiety_w_pakietach.csv")
write.csv(funcje_i_czest_dla_skryptowR(x)[[1]], "funkcje_w_skryptach.csv")
write.csv(funkcje_i_czest_dla_pakietow(x)[[1]], "funkcje_w_pakietach.csv")

pakiety_W_pakietach <- read.csv("pakiety_w_pakietach.csv")
pakiety_W_pakietach <- pakiety_W_pakietach[,-1]

pakiety_w_skryptach <- read.csv("pakiety_w_skryptach.csv")
pakiety_w_skryptach <- pakiety_w_skryptach[,-1]

funkcje_W_pakietach <- read.csv("funkcje_w_pakietach.csv")
funkcje_W_pakietach <- funkcje_W_pakietach[,-1]

funkcje_w_skryptach <- read.csv("funkcje_w_skryptach.csv")
funkcje_w_skryptach <- funkcje_w_skryptach[,-1]
colnames(funkcje_w_skryptach) <- c("funkcje", "czestosc")
library(dplyr)

pakiety <- full_join(pakiety_W_pakietach, pakiety_w_skryptach, by.x ="liczba",by.y="czestosc")
pakiety[is.na(pakiety[,2]),2]<-0L
pakiety[is.na(pakiety[,3]),3]<-0L

funkcje <- full_join(funkcje_W_pakietach, funkcje_w_skryptach, by.x ="liczba",by.y="czestosc")
funkcje[is.na(funkcje[,2]),2]<-0L
funkcje[is.na(funkcje[,3]),3]<-0L

#chmura dla pakietów
slowa <- apply(pakiety[,2:3], 1, sum)
slowa <- data.frame("pakiety" = pakiety$pakiety, "ile"=slowa)
slowa %>% arrange(-ile) -> rysujemy_n 

library(wordcloud2)

wordcloud2(rysujemy_n, color='random-light', size=1)

#chmura dla funkcji
slowa2 <- apply(funkcje[,2:3], 1, sum)
slowa2 <- data.frame("funkcje" = funkcje$funkcje, "ile"=slowa2)
slowa2 %>% arrange(-ile) -> rysujemy2_n 

wordcloud2(rysujemy2_n, color='random-light', size=1)



# boxplot do porownania liczby pakietow ladowanych w pakietach i skryptach

# wyniki geerowane na podstawie pliku .csv uzyskanego w nastepujacy sposob:
write.csv(bib_i_czest_dla_wszystkich_skryptowR(x)[[2]], "pakiety_w_skryptach_liczby.csv")
write.csv(wszystkie_descr(x)[[2]], "pakiety_w_pakietach_liczby.csv")
#gdzie x to lokalizacja pobranych plikow z githuba

pwpl <- read.csv("pakiety_w_pakietach_liczby.csv")
pwpl <- pwpl[,-1]
pwsl <- read.csv("pakiety_w_skryptach_liczby.csv")
pwsl <- pwsl[,-1]

boxplot(pwpl,pwsl, las=1, col = "lightgray")


# boxplot do porownania liczby funkcji uzywanych w pakietach i skryptach

# wyniki generowane na podstawie pliku .csv uzyskanego w nastepujacy sposob:
write.csv(funcje_i_czest_dla_skryptowR(x)[[2]], "funkcje_w_skryptach_liczby.csv")
write.csv(funkcje_i_czest_dla_pakietow(x)[[2]], "funkcje_w_pakietach_liczby.csv")

fwpl <- read.csv("funkcje_w_pakietach_liczby.csv")
fwpl <- fwpl[2]
fwsl <- read.csv("funkcje_w_skryptach_liczby.csv")
fwsl <- fwsl[-1]
which.max(fwpl[[1]])  #1168, usuwamy ta obserwacje dla zwiekszenia czytelnosci

boxplot(fwpl[[1]][-1168],fwsl[[1]], las=1, col = "lightgray")

# histogramy z najczestszymi pakietami/funkcjami wraz z udzialem poszczegolnych rodzajow plikow
pakiety <- full_join(pakiety_W_pakietach, pakiety_w_skryptach, by.x ="liczba",by.y="czestosc")
pakiety[is.na(pakiety[,2]),2]<-0L
pakiety[is.na(pakiety[,3]),3]<-0L
suma <- apply(pakiety[,2:3], 1, sum)

w <- order(suma, decreasing = TRUE)[1:20]
df <- data.frame(pakiet=pakiety[w,1], wystapienia=suma[w], w_pak=pakiety[w,2],w_skryp=pakiety[w,3])

ggplot(df, aes(x=pakiet))+geom_bar(aes(y=wystapienia, fill="udział skryptów"),stat="identity")+geom_bar(aes(y=w_pak, fill="udział pakietów"),stat="identity")+coord_flip()
##
funkcje <- full_join(funkcje_W_pakietach, funkcje_w_skryptach, by.x ="liczba",by.y="czestosc")
funkcje[is.na(funkcje[,2]),2]<-0L
funkcje[is.na(funkcje[,3]),3]<-0L
suma <- apply(funkcje[,2:3], 1, sum)
w <- order(suma, decreasing = TRUE)[1:20]

df <- data.frame(funkcja=funkcje[w,1], wystapienia=suma[w], w_pak=funkcje[w,2],w_skryp=funkcje[w,3])

ggplot(df, aes(x=funkcja))+geom_bar(aes(y=wystapienia, fill="udział skryptów"),stat="identity")+geom_bar(aes(y=w_pak, fill="udział pakietów"),stat="identity")+coord_flip()




# histogram - Porównanie autorów pod względem liczby wykorzystanych pakietów

y <- c( "ncarchedi", "maelle", "leonawicz", "csgillespie", "kislerdm", "fdrennan", 
        "collnell", "longhowlam", "ellisp", "eleakin", "briatte", "jonocarroll", 
        "DavisVaughan", "shrektan", "av1611", "MarcinKosinski", "Bartesto", 
        "AnthonyEbert", "lwjohnst86", "qinwf","TomKellyGenetics", "rmsharp",
        "kieranrcampbell", "eblondel", "kylehamilton", "ndphillips", "leobarone",
        "dpphat",  "haroine", "TaylorAndrew",        "lbartnik", "skirmer",
        "rasmusab", "Nowosad", "edzer", "stufield", "chenwendi", "xprimexinverse",
        "hofnerb", "timtrice", "gabrielzanlorenssi")

pakiety_użytkownik_pakiet <- list()
#jako x podajemy lokalizację plikow z pobranymi danymi
for (i in 1:length(y)) {
  pakiety_użytkownik_pakiet[[i]]<-uzytkownik_pakiety(x,y[i])[[1]]
}
liczba_pak<- numeric(length(y))
for (i in 1:length(y)) {
  liczba_pak[i] <- length(as.character(pakiety_użytkownik_pakiet[[i]]$pakiety))
}
d <- y[order(liczba_pak, decreasing = TRUE)]
l <- sort(liczba_pak, decreasing = TRUE)
df <- data.frame(autor=d, lb_pakietow=l)
ggplot(df, aes(x=autor, y=lb_pakietow))+geom_bar(stat="identity")+coord_flip()+labs(y="liczba wykorzyatnych pakietów przez użytkownika")


#Porównanie autorów pod względem średniej liczby wykorzystanych funkcji w jednym pliku .R
#x jak wcześniej
funkcje <- numeric(length(y))
for (i in 1:length(y)) {
  funkcje[i]<-mean(uzytkownik_funkcje(x,y[i])[[2]])
}

df <- data.frame(autor=y, sr_lb_funkcji_w_pliku = funkcje)

ggplot(df, aes(x=autor, y=sr_lb_funkcji_w_pliku))+geom_bar(stat="identity")+coord_flip()



#############################pliki R + description dla wszystkich pakietow

library(rvest)
adres <- 'https://bioconductor.org/packages/3.7/bioc/'
folder <- 'C:/Users/Ania/Desktop/R dla zaawansowanych/paczki/'

bio <- read_html(adres)
paczki <- html_text(html_nodes(bio, 'a'))
paczki <- paczki[which(paczki=='a4'):which(paczki=='zlibbioc')]


for (i in 1:length(paczki)) {
  pakiet <- read_html(paste(adres, 'html/', paczki[i], '.html', sep=''))
  pakiet <- html_text(html_nodes(pakiet, '.rpack'))[1]
  pakiet <- substr(pakiet, 4, nchar(pakiet))
  
  plik <- tempfile(tmpdir=folder, fileext=".tar.gz")
  download.file(paste(adres, 'src/contrib/', pakiet, sep=''), plik)
  untar(plik, exdir=folder)
  file.remove(plik)
  
  w <- list.files(paste(folder, paczki[i], sep=''))
  for(j in 1:length(w)) {
    if(w[j]!='DESCRIPTION' & w[j]!= 'R')
      unlink(paste(folder, paczki[i], '/', w[j], sep=''), recursive = TRUE)
  }
  
}






############################wykresy


#wykres - czestosc wystepowania funkcji w pakietach

dane <- read.csv('C:/Users/Ania/Desktop/R dla zaawansowanych/projekt1/pakiety_autorzy_funkcje.csv')
dane <- dane[,-1]

funkcje <- dane[,-c(1,2)]

czest_f <- character()
for (i in 1:nrow(funkcje)) {
  a <- funkcje[i,]
  czest_f <- c(czest_f, unique(a[!is.na(a)]))
}
czest_f <- as.factor(czest_f)

czest_f <- as.data.frame(sort(summary(czest_f, maxsum = length(levels(czest_f))), decreasing = TRUE))
czest_f <- cbind(czest_f, round(czest_f/nrow(funkcje), digits = 2))
colnames(czest_f) <- c('ile', 'czestosc')
head(czest_f)
write.csv(czest_f, 'C:/Users/Ania/Desktop/R dla zaawansowanych/projekt1/funkcje_czestosc.csv')


slowa <- read.csv('C:/Users/Ania/Desktop/R dla zaawansowanych/projekt1/funkcje_czestosc.csv', row.names = NULL)
ile_slow <- 10 #parametr podawany przez uzytkownika
slowa <- slowa[1:ile_slow,]
wordcloud::wordcloud(words = slowa[,1], freq = slowa[,3], random.order = FALSE, colors=RColorBrewer::brewer.pal(8, "Accent"))





#wykres - czestosc wystepowania bibliotek w pakietach

dane <- read.csv('C:/Users/Ania/Desktop/R dla zaawansowanych/projekt1/uzyte_pakiety.csv')
bib <- dane[,-c(1,2)]

czest_b <- character()
for (i in 1:nrow(bib)) {
  a <- bib[i,]
  czest_b <- c(czest_b, unique(a[!is.na(a)]))
}
czest_b <- as.factor(czest_b)

czest_b <- as.data.frame(sort(summary(czest_b, maxsum = length(levels(czest_b))), decreasing = TRUE))
czest_b <- cbind(czest_b, round(czest_b/nrow(bib), digits = 2))
colnames(czest_b) <- c('ile', 'czestosc')
head(czest_b)
write.csv(czest_b, 'C:/Users/Ania/Desktop/R dla zaawansowanych/projekt1/biblioteki_czestosc.csv')


slowa <- read.csv('C:/Users/Ania/Desktop/R dla zaawansowanych/projekt1/biblioteki_czestosc.csv', row.names = NULL)
ile_slow <- 50 #parametr podawany przez uzytkownika
slowa <- slowa[1:ile_slow,]
wordcloud::wordcloud(words = slowa[,1], freq = slowa[,3], random.order = FALSE, colors=RColorBrewer::brewer.pal(8, "Accent"))




















##########################

bib <- read.csv('C:/Users/Ania/Desktop/R dla zaawansowanych/projekt1/uzyte_pakiety.csv')
funk <- read.csv('C:/Users/Ania/Desktop/R dla zaawansowanych/projekt1/pakiety_autorzy_funkcje.csv')

head(bib)[1:5]
head(funk)[1:5]


autor_pakiet <- as.data.frame(matrix(c(0,0), nrow = 1))
colnames(autor_pakiet) <- c('autor', 'pakiet')
autor_funkcja<- as.data.frame(matrix(c(0,0), nrow = 1))
colnames(autor_funkcja) <- c('autor', 'funkcja')

n <- nrow(bib)
for (i in 1:n) {
  autor <- as.character(funk[i,2])
  a <- bib[i,3:ncol(bib)]
  a <- unique(a[!is.na(a)])
  if(length(a)>0) {
    for (j in 1:length(a)) {
      autor_pakiet <- rbind(autor_pakiet, c(autor, a[j]))
    }
  }
  b <- funk[i,4:ncol(funk)]
  b <- unique(b[!is.na(b)])
  if(length(b)>0) {
    for (j in 1:length(b)) {
      autor_funkcja <- rbind(autor_funkcja, c(autor, b[j]))
    }
  }
}
autor_funkcja <- unique(autor_funkcja[2:nrow(autor_funkcja),])
autor_pakiet <- unique(autor_pakiet[2:nrow(autor_pakiet),])
write.csv(autor_pakiet, 'C:/Users/Ania/Desktop/R dla zaawansowanych/projekt1/autor_pakiet.csv', row.names = FALSE)
write.csv(autor_funkcja, 'C:/Users/Ania/Desktop/R dla zaawansowanych/projekt1/autor_funkcja.csv', row.names = FALSE)










####################################kategoria B

#czestosc wystepowania funkcji

dane <- read.csv('C:/Users/Ania/Desktop/R dla zaawansowanych/projekt1/B_author_repo_functions.csv')
funkcje <- unique(dane[,-1])

length(czest_f)
czest_f <- funkcje$Functions
czest_f <- as.character(czest_f)
czest_f <- sapply(czest_f, trimws)
czest_f <- sapply(czest_f, function(a) gsub("\\\"", "", a))
czest_f <- sapply(czest_f, function(a) gsub("\\\'", "", a))
czest_f <- as.factor(czest_f)

czest_f <- as.data.frame(sort(summary(czest_f, maxsum = length(levels(czest_f))), decreasing = TRUE))
czest_f <- cbind(czest_f, round(czest_f/nrow(funkcje), digits = 10))
colnames(czest_f) <- c('ile', 'czestosc')

write.csv(czest_f, 'C:/Users/Ania/Desktop/R dla zaawansowanych/projekt1/B_funkcje_czestosc.csv')



#czestosc wystepowania bibliotek

dane <- read.csv('C:/Users/Ania/Desktop/R dla zaawansowanych/projekt1/B_author_repo_packages.csv')
bib <- unique(dane[,-1])

czest_b <- bib$Packages
czest_b <- as.character(czest_b)
czest_b <- sapply(czest_b, trimws)
czest_b <- sapply(czest_b, function(a) gsub("\\\"", "", a))
czest_b <- sapply(czest_b, function(a) gsub("\\\'", "", a))
czest_b <- as.factor(czest_b)

czest_b <- as.data.frame(sort(summary(czest_b, maxsum = length(levels(czest_b))), decreasing = TRUE))
czest_b <- cbind(czest_b, round(czest_b/nrow(bib), digits = 10))
colnames(czest_b) <- c('ile', 'czestosc')
write.csv(czest_b, 'C:/Users/Ania/Desktop/R dla zaawansowanych/projekt1/B_biblioteki_czestosc.csv')












############################# porownanie A i B

#biblioteki

A_bib <- read.csv('C:/Users/Ania/Desktop/R dla zaawansowanych/projekt1/biblioteki_czestosc.csv', row.names = NULL)
B_bib <- read.csv('C:/Users/Ania/Desktop/R dla zaawansowanych/projekt1/B_biblioteki_czestosc.csv', row.names = NULL)

A_bib <- cbind(A_bib[,c(1,3)], as.factor(rep('A', nrow(A_bib))))
colnames(A_bib) <- c('biblioteka', 'czestosc', 'kategoria') 
B_bib <- cbind(B_bib[,c(1,3)], as.factor(rep('B', nrow(B_bib))))
colnames(B_bib) <- c('biblioteka', 'czestosc', 'kategoria') 

razem_b <- rbind(A_bib, B_bib)
head(razem_b)

library(ggplot2)
library(dplyr)

n <- 10 #parametr podawany przez uzytkownika

razem_top_b <- razem_b %>%
  group_by(kategoria) %>%
  top_n(n, czestosc) %>%
  ungroup() %>%
  arrange(kategoria, -czestosc)

razem_top_b %>%
  mutate(biblioteka = reorder(biblioteka, czestosc)) %>%
  ggplot(aes(biblioteka, czestosc, fill = factor(kategoria))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ kategoria, scales = "free") +
  coord_flip() +
  theme(text = element_text(size=15))
        


#funkcje

A_fun <- read.csv('C:/Users/Ania/Desktop/R dla zaawansowanych/projekt1/funkcje_czestosc.csv', row.names = NULL)
B_fun <- read.csv('C:/Users/Ania/Desktop/R dla zaawansowanych/projekt1/B_funkcje_czestosc.csv', row.names = NULL)

A_fun <- cbind(A_fun[,c(1,3)], as.factor(rep('A', nrow(A_fun))))
colnames(A_fun) <- c('funkcja', 'czestosc', 'kategoria') 
B_fun <- cbind(B_fun[,c(1,3)], as.factor(rep('B', nrow(B_fun))))
colnames(B_fun) <- c('funkcja', 'czestosc', 'kategoria') 

head(A_fun)


razem_f <- rbind(A_fun, B_fun)
head(razem_f)

library(ggplot2)
library(dplyr)

n <- 10 #parametr podawany przez uzytkownika

razem_top_f <- razem_f %>%
  group_by(kategoria) %>%
  top_n(n, czestosc) %>%
  ungroup() %>%
  arrange(kategoria, -czestosc)

razem_top_f %>%
  mutate(funkcja = reorder(funkcja, czestosc)) %>%
  ggplot(aes(funkcja, czestosc, fill = factor(kategoria))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ kategoria, scales = "free") +
  coord_flip() +
  theme(text = element_text(size=15))







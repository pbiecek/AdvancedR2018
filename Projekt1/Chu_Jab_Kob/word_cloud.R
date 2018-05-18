library(dplyr)
library(stringr)
library(wordcloud)
library(ggpubr)
library(RColorBrewer) 


# Dane do chmury

tabele<-list.files(getwd(), ".*[^R]$",recursive = T)
Tab<-data.frame()
for(i in 1:length(tabele)){
  Tab<-rbind(Tab,read.table(tabele[i]))
}

mypal<-c(brewer.pal(11, "Spectral"),brewer.pal(8,"Pastel1"))


#                           CHMURA

# chmura dla wszystkich pakietow - funkcje najczesciej uzywane

freq<-Tab%>%
  select(funkcja, liczbaWystapien) %>% group_by(funkcja) %>%count

wordcloud(words = freq$funkcja, freq =freq$n, min.freq = 10,
          max.words = 100, random.order = FALSE, rot.per = 0.35, 
          colors = brewer.pal(12, "Paired"))  


# chmura dla wszystkich pakietow - funkcje najczesciej uzywane poza basem

Tab %>%
  filter(pakiet!="base") %>%
  select(funkcja, liczbaWystapien) %>% group_by(funkcja) %>% count->func_freq

wordcloud(words = func_freq$funkcja, freq = func_freq$n, min.freq = 5,
          max.words = 200, random.order = FALSE, #rot.per = 0.35, 
          colors = mypal )  


#                         Chmura slow w ksztalcie liter

#install.packages("wordcloud2")
#install_github("lchiffon/wordcloud2")
require(devtools)
library(wordcloud2)
freq<-freq%>% arrange(desc(n))
wd<-rbind(freq,freq)
letterCloud(freq, word = "Dziękujemy" ,size = 2)
letterCloud(freq, word = "za" ,size = 0.5)
letterCloud(freq, word = "uwagę!" ,size = 2)


wordcloud2(demoFreq, figPath = figPath, size = 1.5,color = "skyblue")


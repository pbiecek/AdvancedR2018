library(NCmisc)
library(readtext)
list.functions.in.file("C:/Users/Anna Kozak/Downloads/proby.R")



path <- "C:/Users/Anna Kozak/Desktop/warsztaty/AdvancedR/paczki_bioconductor/"
files_in_path <- list.files(path)

library(qdap)
library(stringi)
library(dplyr)
funckje.pakiety4 <- list()
for(i in 332:length(files_in_path)){
  path1 <- paste(path, files_in_path[[i]],sep="")
  path2 <- paste(path1,"/R/", sep="")
  tmp <-list()
  for(j in 1:length(list.files(path2,pattern=glob2rx("*.R")))){
    tmp[[j]]<-list.functions.in.file(paste(path2,list.files(path2,pattern=glob2rx("*.R"))[j],sep=""))
  }
  funckje.pakiety4[[i]] <- unlist(unlist(tmp))
  print(i)
}

##byly jakies bledy, przerywalo co jakis czas
##polaczenie wynikow  w calosc

#pierwsze 159
funckje.pakiety[[1]]
#160-187
funckje.pakiety1[[1]]
#190-259
funckje.pakiety2[[1]]
#261-330
funckje.pakiety3[[1]]
#332-400
funckje.pakiety4[[389]]


length(funckje.pakiety)+length(funckje.pakiety1)+length(funckje.pakiety2) + length(funckje.pakiety3) + length(funckje.pakiety4)
df <- plyr::ldply(funckje.pakiety, rbind)
d1 <- append(funckje.pakiety, funckje.pakiety1)
d2 <- append(d1, funckje.pakiety2)
d3 <- append(d2, funckje.pakiety3)
d4 <- append(d3, funckje.pakiety4)

df_new <- plyr::ldply(d_list,rbind)
name1 <- unlist(files_in_path[1:159])
name2 <- unlist(files_in_path[160:187])
name3 <- unlist(files_in_path[190:259])
name4 <- unlist(files_in_path[261:330])
name5 <- unlist(files_in_path[332:401])
names <- c(name1, name2, name3,name4,name5)
df_new$nazwa_pakietu <- names
df_new_1 <- df_new[,c(372,2:371)]

autor_1 <- unlist(autor[1:159])
autor_2 <- unlist(autor[160:187])
autor_3 <- unlist(autor[190:259])
autor_4 <- unlist(autor[261:330])
autor_5 <- unlist(autor[332:401])
autor_all <- c(autor_1, autor_2, autor_3,autor_4,autor_5)

df_new_1$autor <- autor_all
df_new_2 <- df_new_1[,c(372,1:371)]


library(stringr)
df_new_2$autor <- gsub(",.*", "",df_new_2$autor)
write.csv(df_new_2, "C:/Users/Anna Kozak/Desktop/warsztaty/AdvancedR/pakiety_autorzy_funkcje.csv")

#lab05 poprawianie kodu
setwd("H:\\Windows7\\Documents\\R dla zaawansowanych\\lab05")

library(readr)
#w kodzie bylo
# pakiety_W_pakietach <- read.csv("pakiety_w_pakietach.csv")
#chcemy sprawdzic czy da sie szybciej wczytac niz read.csv

db <- read.csv2("cos.csv")
db <- read.csv("cos.csv", sep=";")
db <- read.table("cos.csv", sep=";")

db <- readLines("cos.csv", sep=";")
db <- read.delim("cos.csv", sep=";")

db <- read_table2("cos.csv")
db <- read_csv2("cos.csv")



popr <- microbenchmark(
  czyt.1 = { db <- read.csv2("cos.csv") },
  czyt_2 = { db <- read_csv2("cos.csv") },
  times=10L
)
summary(popr)
# expr        min         lq     mean     median         uq        max neval
# 1 czyt.1 15416.0937 15493.9400 15853.29 15942.4375 16099.5573 16150.0012    10
# 2 czyt_2   752.0337   771.0544   816.33   806.0934   839.9902   911.3524    10

boxplot(popr)

#wniosek:
#lepiej uzywac funkcji read_csv (z podkreslnikiem) z ppakietu readr



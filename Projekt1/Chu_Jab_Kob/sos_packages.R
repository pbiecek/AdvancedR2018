library("sos")
library(dplyr)
library(stringi)

setwd("~/Advanced.R")
#install.packages("sos")


Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

unknown <- character(0)

zipy <- list.files("C:/Users/Lenovo/Documents/advanced.R","*.zip$", full.names = T)
zipy
lapply(zipy, unzip)
#unzip(zipy)

#unzipy <- stri_extract(zipy, regex = "(.*)(?=-2018)")
unzipy <- as.vector(unzipy)


#unzipy <- list.files("C:/Users/Lenovo/Documents/advanced.R/Autorzy Github",full.names = T)

luz <- length(unzipy)



for (un in 1: luz){
  
  tabelki_csv <- list.files(unzipy[un],full.names = T)
  tabelki_csv
  lt <- length(tabelki_csv)
  #setwd("C:/Users/Lenovo/Documents/Advanced.R/Autorzy Github")
  setwd(unzipy[un])
  
  for(r in 1:lt){
    
    tabela <- read.table(tabelki_csv[r])
    tabela
    fun_bez_pak <- as.vector(tabela[is.na(tabela$nazwa_pakietu),2])
    fun_bez_pak
    
    
    unifun_bez_pak <- unique(fun_bez_pak)
    n <- length(unifun_bez_pak)
    odp <- rep("nic", times = n)
    
    if(n ==0) next
    for (i in 1:n)
    {
      a <-findFn(unifun_bez_pak[i], maxPages = 3)
      odp[i] <-  Mode(a[,5])
      
    }
    
    pom <- tabela[is.na(tabela$nazwa_pakietu),]
    fund <- tabela[!is.na(tabela$nazwa_pakietu),]
    
    
    pom["nazwa_pakietu"] <- unlist(lapply(fun_bez_pak, function(x) odp[unifun_bez_pak==x]))
    pom[is.na(pom$nazwa_pakietu),1] <- 0
    unknown <- c(unknown, as.vector(fun_bez_pak[pom$nazwa_pakietu==0]))
    unknown <- (unique(unknown))
    
    if(any(pom$nazwa_pakietu==0))
    {pom[pom$nazwa_pakietu==0,1] <- NA}
    pom
    
    
    tabela <-rbind(pom, fund)
    nazwa <- stri_extract_last(tabelki_csv[r], regex = "([^\\/]+$)")
    
    write.table(tabela, nazwa)
  }
  setwd("C:/Users/Lenovo/Documents/advanced.R")
  write.table(unknown,  "brakiNApak")
}

unknown1 <- data.frame("funkcja" = numeric(0), "pakiet" = numeric(0))
unknown

unknown1[nrow(unknown1) + 1,] = c("arc.data2sp", "arcgisbinding")
tablela
findFn("manova.mcglm")

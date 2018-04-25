path <- "C:/Users/Anna Kozak/Desktop/warsztaty/AdvancedR/paczki_bioconductor/"
files_in_path <- list.files(path)
library(stringi)

#### pakiety po słowie Depends
uzyte.pakiety.depends <- list()
for(i in 1:length(files_in_path)){
  path1 <- paste(path, files_in_path[[i]],sep="")
  ddd <- unlist(readLines(paste(path,files_in_path[[i]],"/DESCRIPTION",sep="")))
  sl <- gsub(",.*","",unlist(strsplit(unlist(rm_between(paste(ddd,sep=" ",collapse = " "), 'Depends:', ':',extract = TRUE))," ")))
  sl <- sl[-length(sl)]
  ktore_wywalic <- is.na(stri_extract_all_words(sl,"[a-zA-Z]"))
  slowa <- sl[!ktore_wywalic]
  ktore_wywalic1 <- is.na(stri_extract_all_regex(slowa,".+\\)"))
  slowa1 <- slowa[ktore_wywalic1]
  uzyte.pakiety.depends[i] <- list(slowa1)
}

uzyte.pakiety.depends.df <- plyr::ldply(uzyte.pakiety.depends, rbind)
uzyte.pakiety.depends.df$nazwa_pakietu <- files_in_path
uzyte.pakiety.depends.df <- uzyte.pakiety.depends.df[,c(ncol(uzyte.pakiety.depends.df), 1:(ncol(uzyte.pakiety.depends.df)-1))]
#### pakiety po słowie Imports
uzyte.pakiety.imports <- list()
for(i in 1:length(files_in_path)){
  path1 <- paste(path, files_in_path[[i]],sep="")
  ddd <- unlist(readLines(paste(path,files_in_path[[i]],"/DESCRIPTION",sep="")))
  sl <- gsub(",.*","",unlist(strsplit(unlist(rm_between(paste(ddd,sep=" ",collapse = " "), 'Imports:', ':',extract = TRUE))," ")))
  sl <- sl[-length(sl)]
  ktore_wywalic <- is.na(stri_extract_all_words(sl,"[a-zA-Z]"))
  slowa <- sl[!ktore_wywalic]
  ktore_wywalic1 <- is.na(stri_extract_all_regex(slowa,".+\\)"))
  slowa1 <- slowa[ktore_wywalic1]
  uzyte.pakiety.imports[i] <- list(slowa1)
}

uzyte.pakiety.imports.df <- plyr::ldply(uzyte.pakiety.imports, rbind)
uzyte.pakiety.imports.df$nazwa_pakietu <- files_in_path
uzyte.pakiety.imports.df <- uzyte.pakiety.imports.df[,c(ncol(uzyte.pakiety.imports.df), 1:(ncol(uzyte.pakiety.imports.df)-1))]



library(dplyr)
uzyte_pakiety <- full_join(uzyte.pakiety.depends.df, uzyte.pakiety.imports.df, by="nazwa_pakietu")
uzyte_pakiety$`1.x` <- ifelse(uzyte_pakiety$`1.x`=="R", NA, as.character(uzyte_pakiety$`1.x`))



nasze_pakiety <- read.csv("C:/Users/Anna Kozak/Desktop/shiny_app_csv/pakiety_autorzy_funkcje.csv")
nasze_pakiety <- nasze_pakiety$nazwa_pakietu


uzyte_pakiety <- uzyte_pakiety[which(uzyte_pakiety$nazwa_pakietu %in% nasze_pakiety),]
write.csv(uzyte_pakiety,"C:/Users/Anna Kozak/Desktop/shiny_app_csv/uzyte_pakiety.csv")

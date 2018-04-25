library(stringr)

# 1.funkcje do wyci¹gania informacji

funkcje <- function(code_line){
  funkcja <- character(0)
  if(grepl("<-function", code_line)){
    funkcja <- gsub("<-function.*","",code_line)
    funkcja <- gsub("\\(.*","",funkcja)
  }
  if(length(funkcja)==0 || funkcja=="") funkcja <- character(0)
  funkcja
}

zmienne <- function(code_line){
  zmienna <- character(0)
  if(grepl("<-", code_line) & !grepl("<-function", code_line)){
    zmienna <- gsub("<-.*","",code_line)
    zmienna <- gsub("\\[.*","",zmienna)
    zmienna <- gsub(".*\\)","",zmienna)
    zmienna <- gsub("\\$.*","",zmienna)
    zmienna <- gsub("<.*","",zmienna)
    zmienna <- gsub(".*\\(","",zmienna)
  }
  if(length(zmienna)==0 || zmienna=="") zmienna <- character(0)
  zmienna
}

pakiety <- function(code_line){
  code_line <- gsub("\"", "", code_line)
  code_line <- gsub("\'", "", code_line)
  funkcje_do_pakietow <- c("library\\(", 
                           "install.packages\\(", 
                           "require\\(",
                           "requireNamespace\\(",
                           "loadNamespace\\(")
  pakiet <- character(0)
  for(i in funkcje_do_pakietow){
    if(grepl(i, code_line)){
      pakiet <- gsub(paste0(".*",i),"",code_line)
      pakiet <- gsub("\\).*","",pakiet)
      pakiet <- gsub(",.*","",pakiet)
    }
  }
  if(grepl("::", code_line)){
    pakiet <- str_extract(code_line, pattern = '[A-z]*(?=\\::)')
    pakiet <- gsub(".*\\[","",pakiet)
    pakiet <- gsub(".*\\]","",pakiet)
  }
  if(length(pakiet)==0 || pakiet=="") pakiet <- character(0)
  pakiet
}

# 2. funkcja do odczytu pliku

znajdz_informacje <- function(file){ 
  con <- file(description=file, open="r")
  wynik <- list(funkcje=character(),
                zmienne=character(),
                pakiety=character())
  while(TRUE){
    tmp <- readLines(con = con, n = 1)
    if(length(tmp)<=0) break
    
    tmp <- gsub("#.*","",tmp)
    tmp <- gsub("[[:space:]]","",tmp)
    
    tmp_funkcje <- funkcje(tmp)
    if(length(tmp_funkcje)!=0) wynik$funkcje <- append(wynik$funkcje, tmp_funkcje)
    
    tmp_zmienne <- zmienne(tmp)
    if(length(tmp_zmienne)!=0) wynik$zmienne <- append(wynik$zmienne, tmp_zmienne)
    
    tmp_pakiety <- pakiety(tmp)
    if(length(tmp_pakiety)!=0) wynik$pakiety <- append(wynik$pakiety, tmp_pakiety)
  }
  close(con)
  wynik$funkcje <- unique(wynik$funkcje)
  wynik$zmienne <- unique(wynik$zmienne)
  wynik$pakiety <- unique(wynik$pakiety)
  wynik
}


library(stringi)
library(rvest)

###################### pobieranie plikow R z pakietow od autora #########################

# y - nazwa autora
# z - adres katalogu do ktorego maja byc zapisane pliki w formacie takim jak do setwd(),


R_pakietyR <- function(y, z){
  
  x <- stri_replace_all("https://github.com/qinwf?utf8=%E2%9C%93&tab=repositories&q=&type=source&language=r", y, fixed ="qinwf")
    
  link_autor <- read_html(x)
  pakiety <- html_nodes(link_autor, "h3")
  pakiety <- html_text(pakiety)
  l <- length(pakiety)
  
  pakiety1 <- list()

  for (i in 1:l) {
    pakiety1[[i]] <- stri_extract_all_regex(pakiety[i], "\\w+(-*|.*)\\w+(-*|.*)\\w*(-*|.*)\\w*")
  }
  pakiety1 <- unlist(pakiety1)
   
  adres_folderu <- stri_paste("https://github.com/", y, "/", pakiety1)
  czy_pakiet <- list()
  
  for(k in 1:l){
    rrr <- read_html(adres_folderu[k])
    fff <- html_nodes(rrr, ".content")
    fff <- html_text(fff)
    uuu <- unlist(stri_extract_all_words(fff))
    
    czy_pakiet[[k]] <- any(uuu=='DESCRIPTION')
    
  }
  
  czy_pakiet <- unlist(czy_pakiet)
  ktory_pakiet <- which(czy_pakiet==TRUE)
  pakiety2 <- pakiety1[ktory_pakiet]
  nie_pakiety <- pakiety1[-ktory_pakiet]
  
  if(length(pakiety2)>0){

  ######## wyciaganie skryptow R ze wszystkich pakietow ##########

  link <- stri_paste("https://github.com/", y, "/", "fitbitr","/tree/master/R")
  link <- stri_replace_all(link, pakiety2, fixed ="fitbitr")

  for (j in 1:length(link)) {

    a <- read_html(link[j])
    foldery <- html_nodes(a,".content")
    c <- html_text(foldery)
    pliki_i_NA <- unlist(stri_extract_all_regex(c, "\\w+(-*|.*)\\w+(-*|.*)\\w*(-*|.*)\\w*\\.(r|R)"))
    ktore_NA <- which(is.na(pliki_i_NA))
    pliki <- pliki_i_NA[-ktore_NA]

    link[j] <-  stri_replace_all(link[j], "/", fixed="/tree/")
    link[j] <- stri_replace_all(link[j], "raw.githubusercontent", fixed = "github")

    if(length(pliki)>0){
    for (i in 1:length(pliki)) {

      adres <- stri_paste(link[j], "/", pliki[i])
      adres <- unlist(adres)
      setwd(z)
      nazwa <- stri_paste(y, as.character(j), "_", as.character(i), "pakietR")
      writeLines(readLines(adres), nazwa)

    }
    }


  }

}
  
  
}


R_pakietyD <- function(y, z){
  
   x <- stri_replace_all("https://github.com/qinwf?utf8=%E2%9C%93&tab=repositories&q=&type=source&language=r", y, fixed ="qinwf")
  
  link_autor <- read_html(x)
  pakiety <- html_nodes(link_autor, "h3")
  pakiety <- html_text(pakiety)
  l <- length(pakiety)
  
  pakiety1 <- list()
  
  for (i in 1:l) {
    pakiety1[[i]] <- stri_extract_all_regex(pakiety[i], "\\w+(-*|.*)\\w+(-*|.*)\\w*(-*|.*)\\w*")
  }
  pakiety1 <- unlist(pakiety1)


  adres_folderu <- stri_paste("https://github.com/", y, "/", pakiety1)
  czy_pakiet <- list()
  
  for(k in 1:l){
    rrr <- read_html(adres_folderu[k])
    fff <- html_nodes(rrr, ".content")
    fff <- html_text(fff)
    uuu <- unlist(stri_extract_all_words(fff))
    
    czy_pakiet[[k]] <- any(uuu=='DESCRIPTION')
    
  }
  
  czy_pakiet <- unlist(czy_pakiet)
  ktory_pakiet <- which(czy_pakiet==TRUE)
  pakiety2 <- pakiety1[ktory_pakiet]
  nie_pakiety <- pakiety1[-ktory_pakiet]
  
  if(length(pakiety2)>0){
    
    ########### wyciaganie plikow DESCRIPTIONS z pakietow wszystkich ##############
    
    adresy_descr <- stri_paste("https://raw.githubusercontent.com/", y, "/", pakiety2, "/master/DESCRIPTION")
    
    for (i in 1:length(adresy_descr)) {
      
      setwd(z)
      nazwa <- stri_paste(y, "_", as.character(i), "DESCRIPTION")
      writeLines(readLines(adresy_descr[i]), nazwa)
      
    }
  }
  
  
}



R_pakietyA <- function(y, z){
  
  x <- stri_replace_all("https://github.com/qinwf?utf8=%E2%9C%93&tab=repositories&q=&type=source&language=r", y, fixed ="qinwf")
  
  link_autor <- read_html(x)
  pakiety <- html_nodes(link_autor, "h3")
  pakiety <- html_text(pakiety)
  l <- length(pakiety)
  
  pakiety1 <- list()
  
  for (i in 1:l) {
    pakiety1[[i]] <- stri_extract_all_regex(pakiety[i], "\\w+(-*|.*)\\w+(-*|.*)\\w*(-*|.*)\\w*")
  }
  pakiety1 <- unlist(pakiety1)

    adres_folderu <- stri_paste("https://github.com/", y, "/", pakiety1)
  czy_pakiet <- list()
  
  for(k in 1:l){
    rrr <- read_html(adres_folderu[k])
    fff <- html_nodes(rrr, ".content")
    fff <- html_text(fff)
    uuu <- unlist(stri_extract_all_words(fff))
    
    czy_pakiet[[k]] <- any(uuu=='DESCRIPTION')
    
  }
  
  czy_pakiet <- unlist(czy_pakiet)
  ktory_pakiet <- which(czy_pakiet==TRUE)
  pakiety2 <- pakiety1[ktory_pakiet]
  nie_pakiety <- pakiety1[-ktory_pakiet]
  
  
  if(length(nie_pakiety)>0){
    ################# wyciaganie skryptow z analiz i raportow #############
    
    
    link2 <- stri_paste("https://github.com/", y, "/", "fitbitr")
    link2 <- stri_replace_all(link2, nie_pakiety, fixed ="fitbitr")

    for (j in 1:length(link2)) {
      
      a <- read_html(link2[j])
      foldery <- html_nodes(a,".content")
      c <- html_text(foldery)
      pliki_i_NA <- unlist(stri_extract_all_regex(c, "\\w+(-*|.*)\\w+(-*|.*)\\w*(-*|.*)\\w*\\.(Rproj|(r|R))[^md]"))
      pliki_i_NA <- unlist(stri_extract_all_regex(pliki_i_NA, "\\w+(-*)\\w+"%s+%"\\.(Rproj|(r|R))"))
      ktore_NA <- which(is.na(pliki_i_NA))
   pliki <- pliki_i_NA[-ktore_NA]

      
 
      link2[j] <- stri_replace_all(link2[j], "raw.githubusercontent", fixed = "github")
      
      if(length(pliki)>0){
      for (i in 1:length(pliki)) {
        
        adres <- stri_paste(link2[j], "/master/", pliki[i])
        setwd(z)
        nazwa <- stri_paste(y, as.character(j), "_", as.character(i), "skryptR")
        do_zapisu <- readLines(adres)
        writeLines(do_zapisu, nazwa)
        
      }
      }
    }
  }
  
}

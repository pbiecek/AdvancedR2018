### wyszukiwanie bibliotek ###

# x adres katalogu
library(stringi)
library(dplyr)

bib_i_czest_dla_wszystkich_skryptowR <- function(x){
  
  lista_plikow_folder <- list.files(x, recursive = TRUE, full.names = TRUE)
  u <- stri_extract_all_regex(lista_plikow_folder, "\\w+skryptR")
  u <- unlist(u)
  w <-is.na(u)
  pliki_ze_skryptami <- u[-which(w)]
  adresy <- stri_paste(x,"/",pliki_ze_skryptami)
  
  pakiety <- character()
  liczba_pakietow_w_skrypcie <- numeric(length(adresy))
  k <- 1
  
  for (i in 1:length(adresy)) {
    
    plik <- readLines(adresy[i])
    biblioteki <- stri_extract_all_regex(plik, "library\\(.+\\)")
    biblioteki <- unlist(biblioteki)
    xx <- which(is.na(biblioteki)==FALSE)
    hurra <- biblioteki[xx]
    n <- length(hurra)
    if(n>0){
      pakiety[k:(k+n-1)] <- hurra
      k <- k+n
    }
    biblioteki2 <- stri_extract_all_regex(plik, "require\\(.+\\)")
    biblioteki2 <- unlist(biblioteki2)
    xx2 <- which(is.na(biblioteki2)==FALSE)
    hurra2 <- biblioteki2[xx2]
    n2 <- length(hurra2)
    if(n2>0){
      pakiety[k:(k+n2-1)] <- hurra2
      k <- k+n2
    }
    liczba_pakietow_w_skrypcie[i] <- n+n2
  }
  
  pak_nazwy <- unlist(stri_extract_all_words(stri_extract_all_regex(pakiety, "\\(.+\\)")))
  
  nazwy <- unique(pak_nazwy)
  lb_pak <- length(nazwy)
  czestosc <- numeric(lb_pak)
  
  for (i in 1:lb_pak) {
    czestosc[i] <- sum(pak_nazwy==nazwy[i])
  }
  
  wynik <- data.frame(pakiety = nazwy, czestosc = czestosc)
  
  return(list(wynik, liczba_pakietow_w_skrypcie))
  
}



### wyszukiwanie funkcji ###

#jako argument podaj adres katalogu z ktorego maja byc pobrane pliki do analizy

funcje_i_czest_dla_skryptowR <- function(x){
  
  lista_plikow_folder <- list.files(x, recursive = TRUE, full.names = TRUE)
  u <- stri_extract_all_regex(lista_plikow_folder, "\\w+skryptR")
  u <- unlist(u)
  w <-which(is.na(u)==FALSE)
  pliki_ze_skryptami <- u[w]
  adresy <- stri_paste(x,"/",pliki_ze_skryptami)
  
  
  funkcje <- character()
  k <- 1
  
  lb_funkcji_w_skrypcie <- numeric(length(adresy))
  
  for (i in 1:length(adresy)) {
    
    plik <- readLines(adresy[i])
    f <- stri_extract_all_regex(plik, "\\w+\\(")
    f <- unlist(f)
    xx <- which(is.na(f)==FALSE)
    hurra <- f[xx]
    n <- length(hurra)
    if(n>0){
      funkcje[k:(k+n-1)] <- hurra
      k <- k+n
    }
    lb_funkcji_w_skrypcie[i] <- n
  }
  
  f_nazwy <- unlist(stri_extract_all_words(funkcje))
  ww <- which(is.na(f_nazwy)==FALSE)
  f_nazwy <- f_nazwy[ww]
  
  nazwy <- unique(f_nazwy)
  lb_f <- length(nazwy)
  czestosc <- numeric(lb_f)
  
  for (i in 1:lb_f) {
    czestosc[i] <- sum(f_nazwy==nazwy[i])
  }
  
  wynik <- data.frame(funkcje=nazwy, liczba = czestosc)
  
  return(list(wynik, lb_funkcji_w_skrypcie))
  
}

#############  DESCRIPTIONS ##################

#funkcja do wyciagania pakietow z jednego pliku DESCRIPTIONS
#jako argument podaj adres pliku DESCRIPTIONS z pakietu

pakiety_pakiety <- function(x){
  
  descr <- readLines(x)
  
  import <- which(descr==stri_subset_regex(descr, "Imports"))
  if(length(import)>0){
  dwukropki <- stri_subset(descr, regex=":")
  import2 <- which(dwukropki==stri_subset_regex(descr, "Imports"))
  po_impor <- ifelse(is.na(dwukropki[import2+1]), length(descr)+1, which(descr==dwukropki[import2+1]))
  pakiety <- unlist(stri_extract_all_words(descr[(import):(po_impor-1)]))
  pakiety <- pakiety[-which(pakiety=="Imports")]
  w <- which(is.na(pakiety)==FALSE)
  pakiety <- pakiety[w]
  return(pakiety)
  }
}

wszystkie_descr <- function(x){
  
  lista_plikow_folder <- list.files(x, recursive = TRUE, full.names = TRUE)
  u <- stri_extract_all_regex(lista_plikow_folder, "\\w+DESCRIPTION")
  u <- unlist(u)
  w <-which(is.na(u)==FALSE)
  pliki_desc <- u[w]
  adresy <- stri_paste(x,"/",pliki_desc)
  
  
  pakiety <- character()
  k <- 1
  lb_pakietow_w_desc <- numeric(length(adresy))
  
  for (i in 1:length(adresy)) {
  
    hurra <- pakiety_pakiety(adresy[i])
    n <- length(hurra)
    if(n>0){
      pakiety[k:(k+n-1)] <- hurra
      k <- k+n
    }
    lb_pakietow_w_desc[i] <- n
  }
  
  nazwy <- unique(pakiety)
  lb_f <- length(nazwy)
  czestosc <- numeric(lb_f)
  
  for (i in 1:lb_f) {
    czestosc[i] <- sum(pakiety==nazwy[i])
  }
  
  wynik <- data.frame(pakiety=nazwy, liczba = czestosc)
  
  return(list(wynik,lb_pakietow_w_desc))
}



############ funkcje dla pakietow ############

funkcje_i_czest_dla_pakietow <- function(x){
  
  lista_plikow_folder <- list.files(x, recursive = TRUE, full.names = TRUE)
  u <- stri_extract_all_regex(lista_plikow_folder, "\\w+pakietR")
  u <- unlist(u)
  w <-is.na(u)
  pliki_ze_skryptami <- u[-which(w)]
  adresy <- stri_paste(x,"/",pliki_ze_skryptami)
  
  
  funkcje <- character()
  k <- 1
  lb_funkcji_w_pakiecie_pliku <- numeric(length(adresy))
  
  for (i in 1:length(adresy)) {
    
    plik <- readLines(adresy[i])
    f <- stri_extract_all_regex(plik, "\\w+\\(")
    f <- unlist(f)
    xx <- which(is.na(f)==FALSE)
    hurra <- f[xx]
    n <- length(hurra)
    if(n>0){
      funkcje[k:(k+n-1)] <- hurra
      k <- k+n
    }
    lb_funkcji_w_pakiecie_pliku[i] <- n
  }
  
  f_nazwy <- unlist(stri_extract_all_words(funkcje))
  
  nazwy <- unique(f_nazwy)
  lb_f <- length(nazwy)
  czestosc <- numeric(lb_f)
  
  for (i in 1:lb_f) {
    czestosc[i] <- sum(f_nazwy==nazwy[i])
  }
  
  wynik <- data.frame(funkcje=nazwy, liczba = czestosc)
  
  return(list(wynik, lb_funkcji_w_pakiecie_pliku))
  
}

#########################################################################
### dla uzytkownikow teraz poszczegolnych pakiety i funkcje #############
#########################################################################

# x - folder z plikami
# y - uzytkownik

uzytkownik_pakiety <- function(x, y){
  
  lista_plikow_folder <- list.files(x, recursive = TRUE, full.names = TRUE)
  pliki_uzyt <- stri_extract_all_regex(lista_plikow_folder, stri_paste(y,"\\w+"))
  pliki_uzyt <- unlist(pliki_uzyt)
  w <-which(is.na(pliki_uzyt)==FALSE)
  pliki_uzyt <- pliki_uzyt[w]
  
  DESCR <- stri_extract_all_regex(pliki_uzyt, "\\w+DESCRIPTION")
  DESCR <- unlist(DESCR)
  ww <- which(is.na(DESCR)==FALSE)
  DESCR <- DESCR[ww]
  
  skrypty <- stri_extract_all_regex(pliki_uzyt, "\\w+skryptR")
  skrypty <- unlist(skrypty)
  www <- which(is.na(skrypty)==FALSE)
  skrypty <- skrypty[www]
  
  
  adresy_desc <- stri_paste(x, "/", DESCR)
  adresy_skrypty<- stri_paste(x, "/", skrypty)
  
  pakiety <- character()
  k <- 1
  lb_pakietow_w_pakiecie <- numeric(length(adresy_desc)+length(adresy_skrypty))
  
  #wyluskanie pakietow z plikow pakietowych
  
  if(length(DESCR) >0){
  
  for (i in 1:length(adresy_desc)) {
    
    hurra <- pakiety_pakiety(adresy_desc[i])
    n <- length(hurra)
    if(n>0){
      pakiety[k:(k+n-1)] <- hurra
      k <- k+n
    }
    lb_pakietow_w_pakiecie[i] <- n
  }
  }
  #wyluskanie pakietow ze skryptow R
  
  pakiety2 <- character()
  l <- 1
  if(length(skrypty)>0){
  for (i in 1:length(adresy_skrypty)) {
    
    plik <- readLines(adresy_skrypty[i])
    biblioteki <- stri_extract_all_regex(plik, "library\\(.+\\)")
    biblioteki <- unlist(biblioteki)
    xx <- which(is.na(biblioteki)==FALSE)
    hurra <- biblioteki[xx]
    n <- length(hurra)
    if(n>0){
      pakiety2[l:(l+n-1)] <- hurra
      l <- l+n
    }
    biblioteki2 <- stri_extract_all_regex(plik, "require\\(.+\\)")
    biblioteki2 <- unlist(biblioteki2)
    xx2 <- which(is.na(biblioteki2)==FALSE)
    hurra2 <- biblioteki2[xx2]
    n2 <- length(hurra2)
    if(n2>0){
      pakiety2[l:(l+n2-1)] <- hurra2
      l <- l+n2
    }
    lb_pakietow_w_pakiecie[length(adresy_desc)+i] <- n+n2
  }
  
  pakiety1 <- unlist(stri_extract_all_words(stri_extract_all_regex(pakiety2, "\\(.+\\)")))
  }
  if(length(skrypty)>0 & length(DESCR) >0){
  pakiet <- c(pakiety, pakiety1)}else{
    if(length(DESCR)>0){pakiet <- pakiety}else{pakiet <- pakiety1}
  }
  ww <- which(is.na(pakiet)==FALSE)
  pakiet <- pakiet[ww]

  nazwy <- unique(pakiet)
  lb_p <- length(nazwy)
  czestosc <- numeric(lb_p)
  
  for (i in 1:lb_p) {
    czestosc[i] <- sum(pakiet==nazwy[i])
  }
  
  wynik <- data.frame(pakiety=nazwy, liczba = czestosc)
  
  return(list(wynik,lb_pakietow_w_pakiecie))
}


uzytkownik_funkcje <- function(x,y){
  
  lista_plikow_folder <- list.files(x, recursive = TRUE, full.names = TRUE)
  pliki_uzyt <- stri_extract_all_regex(lista_plikow_folder, stri_paste(y,"\\w+"))
  pliki_uzyt <- unlist(pliki_uzyt)
  w <-is.na(pliki_uzyt)
  pliki_uzyt <- pliki_uzyt[-which(w)]
  
  skrypty <- stri_extract_all_regex(pliki_uzyt, "\\w+skryptR")
  skrypty <- unlist(skrypty)
  w <-is.na(skrypty)
  skrypty <- skrypty[-which(w)]
  
  pakiety <- stri_extract_all_regex(pliki_uzyt, "\\w+pakietR")
  pakiety <- unlist(pakiety)
  w <-is.na(pakiety)
  pakiety <- pakiety[-which(w)]
  
  
  adresy_pakiety <- stri_paste(x, "/", pakiety)
  adresy_skrypty<- stri_paste(x, "/", skrypty)
  

  funkcje <- character()
  k <- 1
  lb_funkcji_w_pliku <- numeric(length(adresy_pakiety)+length(adresy_skrypty))
  
  if(length(pakiety)>0){
  for (i in 1:length(adresy_pakiety)) {
    
    plik <- readLines(adresy_pakiety[i])
    f <- stri_extract_all_regex(plik, "\\w+\\(")
    f <- unlist(f)
    xx <- which(is.na(f)==FALSE)
    hurra <- f[xx]
    n <- length(hurra)
    if(n>0){
      funkcje[k:(k+n-1)] <- hurra
      k <- k+n
    }
    lb_funkcji_w_pliku[i] <- n
  }}
  
  if(length(skrypty)>0){
    for (i in 1:length(adresy_skrypty)) {
      
      plik <- readLines(adresy_skrypty[i])
      f <- stri_extract_all_regex(plik, "\\w+\\(")
      f <- unlist(f)
      xx <- which(is.na(f)==FALSE)
      hurra <- f[xx]
      n <- length(hurra)
      if(n>0){
        funkcje[k:(k+n-1)] <- hurra
        k <- k+n
      }
      lb_funkcji_w_pliku[i+length(adresy_pakiety)] <- n
    }
  }
  
  
  
  f_nazwy <- unlist(stri_extract_all_words(funkcje))
  ww <- which(is.na(f_nazwy)==FALSE)
  f_nazwy <- f_nazwy[ww]
  
  nazwy <- unique(f_nazwy)
  lb_f <- length(nazwy)
  czestosc <- numeric(lb_f)
  
  for (i in 1:lb_f) {
    czestosc[i] <- sum(f_nazwy==nazwy[i])
  }
  
  wynik <- data.frame(funkcje=nazwy, liczba = czestosc)
  
  return(list(wynik,lb_funkcji_w_pliku))
}



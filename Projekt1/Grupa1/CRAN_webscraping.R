library(rvest)
library(stringi)
library(dplyr)

# CRAN


setwd("C:/Users/Kasia1/Magisterka/R_zaaw/pakiety2/")

# funkcja do pobierania z jednej strony

k <- 50
wynik <- list(autorzy =rep("0",k),
              nazwy_pakietow = rep("0",k),
              nazwa_wersji = rep("0",k),
              strona = rep("0",k))


pobierz_ze_strony <- function(x, wynik){
  k <- max(which(wynik[[1]] != "0"))
  if(k == -Inf) k=0
  print(k)
  print(paste0("dlugosc wektora:", length(wynik[[1]])) )
  if(k < length(wynik[[1]])){
    # strona z ktorej chcemy pobrac pakiet
    cran_html <- read_html(paste0("https://cran.r-project.org/web/packages/", x,"/index.html"))
    
    wynik[[4]][k+1] <- paste0("https://cran.r-project.org/web/packages/", x,"/index.html")
    
    # wykrycie i zapisanie, kto jest autorem pakietu
    authors_data <- cran_html %>% 
      html_nodes('td') %>% 
      html_text()
    wynik[[1]][k+1] <- authors_data[which(stri_detect_fixed(authors_data,"Maintainer"))+1]
    
    wynik[[2]][k+1]<-x
    
    # wykrycie pliku z pakietem
    pages_data <- cran_html %>% 
      html_nodes('a') %>% 
      html_text()
    plik_pobierz <- stri_trim_both(pages_data[which(stri_detect_fixed(pages_data,".gz"))])
    
    wynik[[3]][k+1] <- plik_pobierz
    
    # pobranie pliku
    download.file(paste0("https://cran.r-project.org/src/contrib/",plik_pobierz), destfile = paste0("./", plik_pobierz))
    
    
  }
  return(wynik)
  
}


pobierz_zaleznosci <- function(start, wynik){
  if(! start %in% wynik[[2]]){
    wynik <- pobierz_ze_strony(start,wynik)
  }
  
  # wykrcie innych pakietow pojawiajacych sie na stronie  
  cran_html <- read_html(paste0("https://cran.r-project.org/web/packages/", start,"/index.html"))
  page_data <- cran_html %>% 
    html_nodes('td') %>% 
    html_text()
  
  suggested <- page_data[which(stri_detect_regex(page_data,"[Ss]uggest"))+1]
  suggested <- paste(suggested, collapse=", ")
  
  
  suggested <- stri_split_regex(suggested, pattern = "\\s|,")
  suggested <- as.vector(sapply(suggested, function(x) stri_trim_both(x)))
  suggested <- suggested[suggested!=""]
  
  
  for( name in suggested){
    # sprawdzenie czy dany pakiet zostal juz pobrany
    if(! name %in% wynik[[2]]){
      print(name)
      tryCatch({
        wynik <- pobierz_ze_strony(name,wynik)
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
      
    }
    
    
    
    
  }
  k <- max(which(wynik[[1]] != "0"))
  if(k < length(wynik[[1]])) {
    wynik <- pobierz_zaleznosci(suggested[1], wynik)
    
    
  }
  
  
  return(wynik)
}


# pobranie róznych bibliotek i ich zale¿nosci
pobierz_ze_strony("Hmisc", wynik)

wynik2 <- pobierz_zaleznosci("survival", wynik)

k <- 50
wynik <- list(autorzy =rep("0",k),
              nazwy_pakietow = rep("0",k),
              nazwa_wersji = rep("0",k),
              strona = rep("0",k))
wynik3 <- pobierz_zaleznosci("gam", wynik)
wynik4 <- pobierz_zaleznosci("stringi", wynik)
wynik3 <- pobierz_zaleznosci("colorspace", wynik)
wynik5 <- pobierz_zaleznosci("gtable", wynik)
wynik6 <- pobierz_zaleznosci("RODBC", wynik)
wynik7 <- pobierz_zaleznosci("DT", wynik)
wynik8 <- pobierz_zaleznosci("car", wynik)
wynik9 <- pobierz_zaleznosci("mthapower",wynik)
wynik10 <- pobierz_zaleznosci("httr",wynik)

wynik12 <- pobierz_zaleznosci("randomForest",wynik)
wynik13 <- pobierz_zaleznosci("vcd",wynik)
tail(wynik13,2)

k <- 10
wynik <- list(autorzy =rep("0",k),
              nazwy_pakietow = rep("0",k),
              nazwa_wersji = rep("0",k),
              strona = rep("0",k))
wynik11 <- pobierz_zaleznosci("forecast",wynik)

for(i in 2:13){
  wynik2 <- rbind(data.frame(wynik2),data.frame(get(paste0("wynik", i))))
  
}
str(wynik2)

wynik2 <- wynik2 %>% 
  mutate(autorzy = as.character(autorzy),
         nazwy_pakietow = as.character(nazwy_pakietow),
         nazwa_wersji = as.character(nazwa_wersji),
         strona =as.character(strona))




# usuniecie duplikatow
wynik_ost <- wynik2  %>% unique()
dim(wynik_ost)


write.csv(wynik_ost,"C:\\Users\\Kasia1\\Magisterka\\R_zaaw\\pakiety2\\pakiety_extract\\info_pakiety_incomplete.csv")


for( pakiet in lista_pakietow){
  
  x <- substr(pakiet, start = 3, stop = stri_locate_first_fixed(pakiet,"_")[,1]-1)
  cran_html <- read_html(paste0("https://cran.r-project.org/web/packages/", x,"/index.html"))
  print(x)
  print(k)
  wynik_ost[[4]][k+1] <- paste0("https://cran.r-project.org/web/packages/", x,"/index.html")
  
  # wykrycie i zapisanie, kto jest autorem pakietu
  authors_data <- cran_html %>% 
    html_nodes('td') %>% 
    html_text()
  wynik_ost[[1]][k+1] <- authors_data[which(stri_detect_fixed(authors_data,"Maintainer"))+1]
  
  wynik_ost[[2]][k+1]<-x
  
  # wykrycie pliku z pakietem
  pages_data <- cran_html %>% 
    html_nodes('a') %>% 
    html_text()
  plik_pobierz <- stri_trim_both(pages_data[which(stri_detect_fixed(pages_data,".gz"))])
  
  wynik_ost[[3]][k+1] <- plik_pobierz
  k=k+1
}




wynik2 <- data.frame(wynik2)


# wypakowanie pakietów i zapis plików txt
lista_pakietow <- list.files(".", full.names = TRUE)
length(lista_pakietow)
pakiet <- lista_pakietow[1]

which(stri_detect_regex(lista_pakietow, "AlphaVantageClient"))

substr(pakiet, start = stri_locate_first_fixed(pakiet, "/")+1, stop = stri_locate_first_fixed(pakiet, "_") -1)

substr(nazwa_pliku, start = stri_locate_last_fixed(nazwa_pliku, "/")+1, stop = stri_length(nazwa_pliku))

for (pakiet in lista_pakietow){
  nazwy_plikow <- untar(pakiet, compressed = 'gzip', list = TRUE)
  nazwy_plikow_r <- nazwy_plikow[stri_detect_regex(nazwy_plikow, "/R/.+")]
  nazwy_plikow_r <- nazwy_plikow_r[stri_detect_regex(nazwy_plikow_r, ".R$")]
  
  
  for( nazwa_pliku in nazwy_plikow_r){
    untar(pakiet, files =  nazwa_pliku, exdir = "./pakiety_extract/")
    tresc_pliku <- readLines(paste0("./pakiety_extract/",nazwa_pliku))
    write(tresc_pliku,
          paste0("./pakiety_extract/","A_", substr(pakiet, start = stri_locate_first_fixed(pakiet, "/")+1, stop = stri_locate_first_fixed(pakiet, "_") -1),
                 "_",stri_replace(substr(nazwa_pliku, start = stri_locate_last_fixed(nazwa_pliku, "/")+1, stop = stri_length(nazwa_pliku)), fixed = ".R",replacement = ""), ".txt"))
    file.remove(paste0("./pakiety_extract/",nazwa_pliku))
    
    
  }
}

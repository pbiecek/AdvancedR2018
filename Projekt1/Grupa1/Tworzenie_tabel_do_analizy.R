setwd("D:/MY DATA/STUDIA/MGR Sem 4/R dla Zaawansowanych/Projekt 1/")

# trzeba mieć folder "summary" z wyciągniętymi informacjami w folderach z nazwami autorów 

LISTA_TABEL <- vector("list", length = 6)

licznik <- 1

for(grupa in c("A", "B")) {
  
  autorzy <- list.files(paste0("./summary/",grupa)) # pobieram sobie autorów
  
  for(rodzaj in c("funkcje","pakiety","zmienne")) {
    
    # dla konkretnej grupy i rodzaju, tworzę tabelę, 
    # w której zliczam wystąpienie danego "rodzaju" dla każdego autora
    
    tabela_tmp <- data.frame(matrix(NA, ncol=length(autorzy)+1))[0,]
    colnames(tabela_tmp) <- c(rodzaj, autorzy)
    
    for(autor in autorzy) {
      
      sciezka <- paste0("./summary/", grupa, "/", autor, "/", rodzaj, ".txt")
      
      if (!file.exists(sciezka)) # plik danego rodzaju moze nie istnieć 
        next 
      
      info = file.info(sciezka) 
      if( info$size == 0 ) # albo może być pusty
        next
      
      lista_nazw <- table(sort((readLines(sciezka))))
      
      juz_w_tabeli <- which(names(lista_nazw) %in% tabela_tmp[,rodzaj]) # te już zostaly wpisane do tabeli, nie chce tworzyc nowego wiersza tylko dopisać do istniejace liczbę ich wystapien
      
      # wpisanie tych co już były w tabeli z poprzednich autorów
      indeksy <- which(names(lista_nazw[juz_w_tabeli]) %in% tabela_tmp[,rodzaj])
      tabela_tmp[indeksy,autor] <- lista_nazw[juz_w_tabeli]
      
      # dopisanie nowych
      if(length(lista_nazw)-length(indeksy) != 0) {
        nrow_ <- nrow(tabela_tmp)
        if(length(juz_w_tabeli)==0) {
          tabela_tmp[(nrow_+1):(nrow_+(length(lista_nazw)-length(indeksy))),c(rodzaj,autor)] <- 
            cbind(names(lista_nazw), lista_nazw)
        } else {
          tabela_tmp[(nrow_+1):(nrow_+(length(lista_nazw)-length(indeksy))),c(rodzaj,autor)] <- 
            cbind(names(lista_nazw)[-juz_w_tabeli], lista_nazw[-juz_w_tabeli])
        }
      }
      
      tabela_tmp <- tabela_tmp[order(tabela_tmp[,rodzaj]),]
      
    }
    
    tabela_tmp[is.na(tabela_tmp)] <- 0
    
    for(i in 2:ncol(tabela_tmp)) 
      tabela_tmp[,i] <- as.numeric(tabela_tmp[,i])
    
    LISTA_TABEL[[licznik]] <- tabela_tmp
    names(LISTA_TABEL)[licznik] <- paste0(grupa,"_",rodzaj)
    licznik <- licznik + 1
  }
}

save(LISTA_TABEL, file="LISTA_TABEL.rda")

# obiekt LISTA_TABEL zawiera następujące tabele:
# LISTA_TABEL$A_funkcje
# LISTA_TABEL$A_pakiety
# LISTA_TABEL$A_zmienne
# LISTA_TABEL$B_funkcje
# LISTA_TABEL$B_pakiety
# LISTA_TABEL$B_zmienne
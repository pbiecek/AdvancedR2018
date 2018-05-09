install.packages("rvest")
library(rvest)
library(stringi)


# funkcja pomocnicza do  tworzenia zestawien
list.functions.in.file <- function(filename,alphabetic=TRUE) {
  if(!file.exists(filename)) {
    stop("couldn't find file ",filename) }
  tmp <- getParseData(parse(filename, keep.source=TRUE))
  nms <- tmp$text[which(tmp$token=="SYMBOL_FUNCTION_CALL")]
  funs <- if(alphabetic) { 
    sort(nms) } else { nms }
  src <- paste(as.vector(sapply(funs, find)))
  outlist <- tapply(funs, factor(src), c)
  return(outlist)
}


Tabelki<-function(nick, folder){
  
  # pobieramy repozytoria do folderu
  setwd(folder)
  if (file.exists(nick)){
    setwd(file.path(folder, nick))
  } else {
    dir.create(file.path(folder, nick))
    setwd(file.path(folder, nick))
  }
  adres <- read_html(paste0("https://github.com/",nick,"?utf8=&tab=repositories&q=&type=&language=r"))
 
  # pobieramy imie autora
  aut<-html_nodes(adres, ".overflow-hidden")
  autor<- html_text(aut)

  pom<- html_nodes(adres, "h3") 
  repozytoria <- html_text(pom)
  
  repozytoria<-trimws(repozytoria)
  n<-length(repozytoria)
  
  for(i in 1:n){
    download.file(url=paste0("https://github.com/",nick,"/",repozytoria[i],"/zipball/master/"),
                  destfile=paste0(file.path(folder, nick),"/",repozytoria[i],".zip"))
    
  }
  


    l_tabel<-0 # liczba tabel

for(r in 1:n){
  unzip(paste0(repozytoria[r],".zip"))
}


repozytoria2<-unique(list.dirs(getwd(),recursive=F))

# z pobranych plikow wybieramy do analizy tylko pliki .R
  for(r in 1:length(repozytoria2)){
    funkcje <- data.frame()
    lista<-c(list.files(repozytoria2[r], "*\\.R$", recursive = T,  full.names = TRUE),
             list.files(repozytoria2[r], "*\\.r$", recursive = T, full.names = TRUE))
    n <- length(lista)

    if(n!=0){
      l_tabel<-l_tabel+1
# Sprawdzamy, czy mamy do czynienia z pakietem
      for(i in  1:n ){
        l <-list.files(repozytoria2[i], recursive = T, full.names = T)
        li <- stri_detect(l, fixed = "DESCRIPTION")
        des <- ifelse(any(li), which(li), -1)
 
  
  # dla kazdego pliku z listy tworzymy zestawienia
          tryCatch({
          nazwa <- stri_extract_last(lista[i], regex = "[^/]+$")
          fun <- list.functions.in.file(lista[i], alphabetic = TRUE)
          
          if(length(fun)!=0){
            fun <- lapply(fun, table)
            fun <- lapply(fun, as.data.frame )
            
            fun <- do.call("rbind", fun)
            fun["plik"] <- nazwa
            
            if(des>0) fun["autor"]<-"pakiet" else fun["autor"]<-autor
            funkcje <- rbind(funkcje, fun)
            funkcje <- tibble::rownames_to_column(funkcje)
            colnames(funkcje) <- c("pakiet", "funkcja", "liczbaWystapien" , "plik", "autor")
            funkcje[,1] <- stri_extract(funkcje[,1], regex = "(?<=package:)(.*)(?=\\.)")
            write.table(funkcje,paste(autor, repozytoria[r]))
            }
        }
        , error = function(e) i <-i+1
        )
      }
    }
    
    #usuwamy zipy i foldery
    unlink(repozytoria2[r], recursive=TRUE)
    unlink(repozytoria[r], recursive=TRUE)
    file.remove(paste0(repozytoria[r],".zip"))
  }
cat("Tabelki gotowe!", "Wygenerowano", l_tabel, "tabel dla ", length(repozytoria), "repozytoriów.")
}


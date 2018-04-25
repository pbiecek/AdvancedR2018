library(stringi)
library(dplyr)


#Skrypt analizuj¹cy pliki spakowane do archiwum o rozszerzeniu .tar.gz (np. pakiety pobrane z Crana) 
#Dla ka¿dej paczki .tar.gz tworzony jest plik .txt o tkaiej samej nazwie z list¹ funkcji odnalezionych
# w plikach R oraz informacj¹ z jakiego pakietu pochodz¹ i kto jest autorem tego pakietu.

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


repozytoria <- list.files(getwd(),"*.tar.gz$", full.names = T)
repozytoria
lr <- length(repozytoria)
for(r in 1:lr){

  funkcje <- data.frame()
  sciezka <- paste0(repozytoria[r])
  untar(sciezka)
  lista <- c(list.files(stri_extract(sciezka,regex="[^_]*"), "*\\.R$", recursive = T, full.names = T),
             list.files(stri_extract(sciezka,regex="[^_]*"), "*\\.r$", recursive = T, full.names = T))
  lista
  n <- length(lista)
  
  for( i in  1:n ){
    
    tryCatch({
      nazwa <- stri_extract_last( lista[i], regex = "[^/]+$")

      fun <- list.functions.in.file(lista[i], alphabetic = TRUE)
      fun <- lapply(fun, table)
      fun <- lapply(fun, as.data.frame )
      
      fun <- do.call("rbind", fun)
      fun["plik"] <- nazwa
      if(is.null(ncol(fun))) next
      funkcje <- rbind(funkcje, fun)   }
      , error = function(e) i <-i+1
    )
  }
  funkcje <- tibble::rownames_to_column(funkcje)
  colnames(funkcje) <- c("nazwa_pakietu", "funkcja", "liczba_wystapien", "plik")
 
  funkcje[,1] <- stri_extract(funkcje[,1], regex = "(?<=package:)(.*)(?=\\.)")
  
  
  #sprawd? czy jest desctiption
  l <-list.files(stri_extract(sciezka,regex="[^_]*"), recursive = T, full.names = T)
  li <- stri_detect(l, fixed = "DESCRIPTION")
  des <- ifelse(any(li), which(li), -1)
  
  funkcje <- na.omit(funkcje)
  funkcje$pakiet <- NA

  #
  nazwa <- stri_extract(repozytoria[r], regex = "[^/]+$")
  nazwa <- stri_extract(nazwa,regex="[^_]*")
  nazwa <- ifelse( stri_sub(nazwa, stri_length(nazwa)- 6,stri_length(nazwa) ) =="-master", 
                   stri_sub(nazwa, 1,  stri_length(nazwa)- 7), nazwa)
  
  funkcje$pakiet <- nazwa
  funkcje$autor <- NA
  
    ?write.table
  if (des >0){
    des <- readtext::readtext(l[des])$text
    autor <- trimws(stri_extract(gsub("[\r\n]", ";",des), regex = "Author: [^;]*"))
    autor <- gsub("Author: ","",autor)
    funkcje$autor <- autor
    write.table(funkcje, paste0(nazwa,"_",autor,".txt" ))
  } else{
    #zapisa? z autorem
    autor<- "Ujjwal Karn"
    write.table(funkcje, paste0(nazwa,"_",autor,".txt"))
  }


  unlink(nazwa, recursive=TRUE)
}



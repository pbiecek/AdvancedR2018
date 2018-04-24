# pakiety
library(stringr)

# lokalizacja -> dwa pierwsze trzeba ustawiæ (lokalizacja_main, lokalizacja)
# i plik wyciaganie_informacji.R umieœciæ w lokalizacja_main
lokalizacja_main <- "D:\\SMAD\\R_level2\\projekt1"
lokalizacja_pliki <- paste0(lokalizacja_main,"\\","pakiety_extract_v2\\pakiety_extract") #lokalizacja plików .txt

source(paste0(lokalizacja_main,"\\wyciaganie_informacji.R"))
autorzy_pakietow <- read.csv(paste0(lokalizacja_main,"\\info_pakiety.csv"), row.names = 1, header=TRUE)
autorzy_pakietow$autorzy <- gsub(" <.*", "", autorzy_pakietow$autorzy)
autorzy_pakietow$autorzy <- gsub(" ", "", autorzy_pakietow$autorzy)

autorzy_pakietow2 <- read.csv(paste0(lokalizacja_main,"\\info_pakiety_incomplete.csv"), row.names = 1, header=TRUE)
autorzy_pakietow2$autorzy <- gsub(" <.*", "", autorzy_pakietow2$autorzy)
autorzy_pakietow2$autorzy <- gsub(" ", "", autorzy_pakietow2$autorzy)

autorzy_pakietow <- autorzy_pakietow2[autorzy_pakietow2$autorzy %in% setdiff(autorzy_pakietow2$autorzy, autorzy_pakietow$autorzy),]

lokalizacja_summary <- paste0(lokalizacja_main,"\\summary_Kasia")
if(!file.exists(lokalizacja_summary)) dir.create(file.path(lokalizacja_summary))
if(!file.exists(paste0(lokalizacja_summary,"\\A"))) dir.create(file.path(paste0(lokalizacja_summary,"\\A")))
if(!file.exists(paste0(lokalizacja_summary,"\\B"))) dir.create(file.path(paste0(lokalizacja_summary,"\\B")))
setwd(lokalizacja_summary)

# sciagniete pliki
pliki <- list.files(lokalizacja_pliki)



for(i in pliki[14913:length(pliki)]){
  group <- substring(i, 1, 1)

  autor_lub_pakiet <- strsplit(i, '_')[[1]][2]
  if(autor_lub_pakiet %in% autorzy_pakietow$nazwy_pakietow){#   ){  
    autor <- unique(autorzy_pakietow$autorzy[autorzy_pakietow$nazwy_pakietow==autor_lub_pakiet])
    lokalizacja_autor_A <- paste0(lokalizacja_summary,"\\A\\",autor)
    if(!file.exists(lokalizacja_autor_A)) dir.create(lokalizacja_autor_A)
    lok_tmp <- lokalizacja_autor_A
  }else{
    autor <- autor_lub_pakiet
    if(group == "A"){
      lokalizacja_autor_A <- paste0(lokalizacja_summary,"\\A\\",autor)
      if(!file.exists(lokalizacja_autor_A)) dir.create(lokalizacja_autor_A)
      lok_tmp <- lokalizacja_autor_A
    }
    if(group == "B"){
      lokalizacja_autor_B <- paste0(lokalizacja_summary,"\\B\\",autor)
      if(!file.exists(lokalizacja_autor_B)) dir.create(lokalizacja_autor_B)
      lok_tmp <- lokalizacja_autor_B
    }
  }
       
  inf <- znajdz_informacje(paste0(lokalizacja_pliki,"\\",i))
       
  con <- file(description=paste0(lok_tmp,"\\zmienne.txt"), open="a+")
  writeLines(inf$zmienne, con, sep = "\n")
  close(con)
  
  con <- file(description=paste0(lok_tmp,"\\funkcje.txt"), open="a+")
  writeLines(inf$funkcje, con, sep = "\n")
  close(con)
  
  con <- file(description=paste0(lok_tmp,"\\pakiety.txt"), open="a+")
  writeLines(inf$pakiety, con, sep = "\n")
  close(con)
}







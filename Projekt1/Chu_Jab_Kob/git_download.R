#install.packages("rvest")
library(rvest)


# 1.       Pobierz repozytoria do folderu

pobierzRepozytoria<-function(nick, folder){
  setwd(folder)
  if (file.exists(nick)){
    setwd(file.path(folder, nick))
  } else {
    dir.create(file.path(folder, nick))
    setwd(file.path(folder, nick))
  }
  
  adres <- read_html(paste0("https://github.com/",nick,"?utf8=&tab=repositories&q=&type=&language=r"))
  
  temp<- html_nodes(adres, "h3") 
  repozytoria <- html_text(temp)
  repozytoria<-trimws(repozytoria)
  n<-length(repozytoria)
  
  for(i in 1:n){
    download.file(url=paste0("https://github.com/",nick,"/",repozytoria[i],"/zipball/master/"),
                  destfile=paste0(file.path(folder, nick),"/",repozytoria[i],".zip"))
    
  }
}

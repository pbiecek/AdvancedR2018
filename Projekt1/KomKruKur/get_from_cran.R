options(stringsAsFactors = FALSE)

library(rvest)
library(httr)
library(jsonlite)
library(dplyr)
library(stringr)



save_from_CRAN <- function(ile_pakietow = Inf,sciezka_do_tar,sciezka_do_kodow){
  
  tab <- read_html('https://cran.r-project.org/web/packages/available_packages_by_name.html') %>% html_nodes('body > table:nth-child(3)') %>% html_table(fill=T) %>% data.frame()
  
  tab <- tab[-1,]
  
  setwd(file.path(sciezka_do_kodow))
  
  for (i in 1:nrow(tab)) {
    p <- tab[i,'X1']
    print(p)
    Sys.sleep(2)
    tryCatch( {
    pagee<- read_html(paste0('https://cran.r-project.org/web/packages/',p,'/index.html')) %>% html_nodes('body > table:nth-child(5)') %>% html_table(fill=T) %>% data.frame() %>% filter(str_detect(X1, 'source:')) %>% select(X2)
    
    
    pagee1 <- paste0('https://cran.r-project.org/src/contrib/',pagee[1,1])
    
    
  
    download.file(pagee1,destfile = paste0(sciezka_do_tar,p,'.tar.gz'),quiet = T)
    
    
    files <- untar(paste0(sciezka_do_tar,p,'.tar.gz'),list = T) 
    files <- files[str_detect(files,'/R/')]
    
    for (fil_name in files){untar(paste0(sciezka_do_tar,p,'.tar.gz'),files = fil_name)}
    
    untar(paste0(sciezka_do_tar,p,'.tar.gz'),files = paste0(p,'/DESCRIPTION'))
  },error = function(e) {})
    if(i > ile_pakietow) {return()}
  }
}



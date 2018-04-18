options(stringsAsFactors = FALSE)

library(rvest)
library(httr)
library(jsonlite)
library(dplyr)
library(stringr)



save_from_GIT <- function(folder,downloaded_repos_limit = Inf,git_login,git_password) {
  
  downloaded_repos <- 0
  
  # Tworze liste uzytkownikow
  
  users <- c()
  
  for (i in 1:2){ # 200 uzytkownikow powinno wystarczyc
  
    url <- paste0('https://api.github.com/search/users?q=type:user+language:r+repos:50..100&per_page=100&page=',i)
    
    res <- GET(url,authenticate(git_login,git_password))
    stop_for_status(res)
    r <- as.data.frame(fromJSON(content(res,'text'), flatten = T))
    users <- c(users,as.vector(r$items.login))
  
  }
  
  
  for (user in users[160:length(users)]) {
    
    repos <- c()
    Sys.sleep(2)
    tryCatch({
    url <- paste0('https://api.github.com/search/repositories?q=user:',user,'+language:r&per_page=100')
    res <- GET(url,authenticate(git_login,git_password))
    if (status_code(res) == 200) {
      r <- as.data.frame(fromJSON(content(res,'text'), flatten = T))
      repos <- c(repos,r$items.name)
      
      if(length(repos) > 0 & !dir.exists(file.path(folder,user))) 
        {dir.create(file.path(folder,user))}
      
      for (repo in repos){
        
        Sys.sleep(2)
        
        res1 <- GET(paste0('https://api.github.com/repos/',user,'/',repo,'/git/trees/master?'),
                    authenticate(git_login,git_password))
        if (status_code(res) == 200) {
          files <- unlist(lapply(content(res1)$tree, "[", "path"), use.names = F)
          files <- files[str_detect(files,'.R$')]
          
          if(length(files) > 0 & !dir.exists(file.path(folder,user,repo))) 
          {dir.create(file.path(folder,user,repo))}
          
          for (fil in files){
            tryCatch(
              download.file(url = 
                  paste0('https://raw.githubusercontent.com/',user,'/',repo,'/master/',fil),
                  destfile = paste0(file.path(folder,user,repo),'/',fil), quiet = TRUE),
              error = function(e) {})
            
          }
          downloaded_repos  <- downloaded_repos + 1
          if (downloaded_repos > downloaded_repos_limit) {return()}
    } } 
  } },error = function(e) {}) }
}



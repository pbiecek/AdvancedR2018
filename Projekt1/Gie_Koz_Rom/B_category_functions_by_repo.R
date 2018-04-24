library(NCmisc)
library(qdap)
library(stringi)
library(dplyr)
library(ggplot2)
library(shiny)

functions_for_author <- function(author_repo_path) {
  funtions_by_repo <- list()
  author_repos <- list.files(author_repo_path)
  for(repo in author_repos) {
    print(paste("   ", repo))
    repo_path = file.path(author_repo_path, repo)
    file_paths <- list.files(repo_path, pattern=glob2rx("*.R"), full.names = TRUE)
    funtions_by_repo[[repo]] = lapply(file_paths, function(file_path) {
      res = NULL
      tryCatch(
        {
          res = list.functions.in.file(file_path)
        },
        error = function(e) {
          print(paste("error in file", file_path))
        })
      return(res)
    })
  }
  return(funtions_by_repo)
}
sources_folder_path <- "B_analysts_sources_github"

authors <- list.files(sources_folder_path)

functions_by_author <- list()
for(author in authors) {
  print(author)
  functions_by_author[[author]] = functions_for_author(file.path(sources_folder_path, author))
}


functions_by_repo <- list()
for (author_name in names(functions_by_author)) {
  #print(author_name)
  author_sources <- functions_by_author[[author_name]]
  source_functions <- list()
  for(author_source_name in names(author_sources)) {
    print(author_source_name)
    source_functions[[author_source_name]] <- as.vector(unlist(author_sources[author_source_name]))
    
  }
  functions_by_repo[[author_name]] <- source_functions
}

functions_by_repo <- functions_by_repo
# zapisanie listy jako RData
save(functions_by_repo , file = "saved_data/functions_by_repo.RData")


# zapisanie do CSV
author_repo_functions <- data.frame(Author=as.Date(character()),
           Repo=character(), 
           Functions=character(), 
           stringsAsFactors=FALSE)

for (author in names(functions_by_repo)) {
  print(author)
  for(repo in names(functions_by_repo[[author]])) {
    functions = paste(functions_by_repo[[author]][[repo]], collape = "", sep = "")
    print(repo)
    author_repo_functions <- rbind(author_repo_functions, data.frame(Author = author, Repo = repo, 
                        Functions = functions))
    
  }
}

write.csv(author_repo_functions,"saved_data/B_author_repo_functions.csv")








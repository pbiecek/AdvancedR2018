library(stringr)
# paczki uzywane przez autorów

packages_used_by_author <- function(author) {
  
}

sources_folder_path <- "B_analysts_sources_github"

authors <- list.files(sources_folder_path)

packages_in_file <- function(file) {
  file<-file(file, "r")
  lines <- readLines(file)
  packages <- c()
  for(line in lines) {
    package <- str_match(line, "^library\\((.*)\\)$")[2]
    if(!is.na(package)) {
      packages <- c(packages, package)
    }
  }
  return(packages)
}

packages_in_file("B_analysts_sources_github/bcaffo/ADtrialPower/server.R")


packages_for_author <- function(author_repo_path) {
  packages_by_repo <- list()
  author_repos <- list.files(author_repo_path)
  for(repo in author_repos) {
    print(paste("   ", repo))
    repo_path = file.path(author_repo_path, repo)
    file_paths <- list.files(repo_path, pattern=glob2rx("*.R"), full.names = TRUE)
    packages_by_repo[[repo]] = unlist(lapply(file_paths, function(file_path) {
      res = NULL
      tryCatch(
        {
          res = packages_in_file(file_path)
        },
        error = function(e) {
          print(paste("error in file", file_path))
        })
      return(res)
    }))
    packages_by_repo[[repo]] = unique(packages_by_repo[[repo]])
  }
  return(packages_by_repo)
}

sources_folder_path <- "B_analysts_sources_github"

authors <- list.files(sources_folder_path)

packages_by_author <- list()
for(author in authors) {
  print(author)
  packages_by_author[[author]] = packages_for_author(file.path(sources_folder_path, author))
}



packages_by_repo <- list()
for (author_name in names(packages_by_author)) {
  #print(author_name)
  author_sources <- packages_by_author[[author_name]]
  source_functions <- list()
  for(author_source_name in names(author_sources)) {
    print(author_source_name)
    source_functions[[author_source_name]] <- as.vector(unlist(author_sources[author_source_name]))
    
  }
  packages_by_repo[[author_name]] <- source_functions
}
packages_by_repo
save(packages_by_repo, file = "saved_data/packages_by_repo.RData")




# zapisanie do CSV
author_repo_functions <- data.frame(Author=as.Date(character()),
                                    Repo=character(), 
                                    Packages=character(), 
                                    stringsAsFactors=FALSE)

for (author in names(packages_by_repo)) {
  print(author)
  for(repo in names(packages_by_repo[[author]])) {
    functions = paste(packages_by_repo[[author]][[repo]], collape = "", sep = "")
    print(repo)
    author_repo_functions <- rbind(author_repo_functions, data.frame(Author = author, Repo = repo, 
                                                                     Packages = functions))
    
  }
}


write.csv(author_repo_functions,"saved_data/B_author_repo_packages.csv")

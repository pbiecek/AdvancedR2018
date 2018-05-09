# Projekt KomKruKur
# Plik analize_GIT.R

# Poprzednia wersja:

########################
# how many repos has author
########################

df <- db_git %>%
  select(user_name, repo_name) %>%
  group_by(user_name) %>%
  summarise(n_repo=n_distinct(repo_name))

#Wersja poprawiona:

df <- as.data.table(db_git)[, .(n_repo = uniqueN(repo_name)) , by = user_name]

# Microbenchmark

microbenchmark( dplyr = {db_git %>%
                         select(user_name, repo_name) %>%
                         group_by(user_name) %>%
                         summarise(n_repo=n_distinct(repo_name))},
                 data.table = as.data.table(db_git)[, .(n_repo = uniqueN(repo_name)) , by = user_name] )


#Unit: milliseconds
#expr        min       lq        mean      median   uq        max        neval   cld
#dplyr       15.057801 15.454802 15.817698 15.57098 15.742384 20.058657  100     b
#data.table  4.086479  4.288507  4.362236  4.37436  4.438601  4.687458   100     a 


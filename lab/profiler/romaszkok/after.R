# Oryginalny plik pod linkiem
# https://github.com/pbiecek/AdvancedR2018/blob/master/Projekt1/Chu_Jab_Kob/cran_download.R
library(rvest)

k <- 4

library(profr)
pro <- profr({
  
  pkgs <- read_html("https://cran.r-project.org/web/packages/available_packages_by_name.html")
  tab_faster <- html_nodes(pkgs, "table tr td a") %>% html_text()
  pkgnames <- tab_faster
  
}, 0.01)
head(pro)
plot(pro)

sapply(pkgnames[1:k], download.packages)

#Skrypt pobieraj¹cy pierwsze k pakietów z serwisu CRAN - R Project

k <- 400

setwd("C:\\Users\\user\\Documents\\PADR\\AdvancedR")

install.packages("rvest")
library(rvest)
pkgs <- read_html("https://cran.r-project.org/web/packages/available_packages_by_name.html")
tab <- html_nodes(pkgs, "table") %>% html_table(fill = TRUE)

pkgnames <- tab[[1]][1]$X1
pkgnames <- pkgnames[nchar(pkgnames)>0]

View(pkgnames)

sapply(pkgnames[1:k], download.packages, destdir="C:\\Users\\user\\Documents\\PADR\\AdvancedR")


#lab01
library("BetaBit")
proton()
db <- employees

db$login[db$name=="John" & db$surname=="Insecure"]

proton(action="login", login="johnins")

top1000passwords

for(pwd in top1000passwords){
  proton(action="login", login="johnins", password=pwd)
}

head(logs)


####
# 3
employees$login[employees$surname=="Pietraszko"]


db <- logs[which(logs$login=="slap"),]
db$host <- as.character(db$host)
# library(dplyr)  
db %>%
  group_by(host) %>%
  count(host)
which.max(table(db$host))


proton(action = "server", host="194.29.178.16")

####
# 4
head(bash_history)

pwds <- gsub( " .*$", "", bash_history )
for(pwd in pwds){
  proton(action="login", login="slap", password=pwd)
}

#http://biecek.pl/BetaBit/Warsaw


########################
------
  
w80dni <- readLines("http://www.gutenberg.org/cache/epub/103/pg103.txt")
head(w80dni)

------
  
library("rvest")
premiery <- read_html("http://www.filmweb.pl/premiere")
filmy <- html_nodes(premiery, ".filmPreview__title")
html_text(filmy)

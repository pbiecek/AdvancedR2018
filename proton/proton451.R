library("BetaBit")
#install.packages("BetaBit")
proton()


employees[employees$name=="John" &  employees$surname=="Insecure",3] 
proton(action = "login", login="johnins")


d<-read.table("https://raw.githubusercontent.com/DavidWittman/wpxmlrpcbrute/master/wordlists/1000-most-common-passwords.txt", sep=";")
n<-nrow(d)

for(i in 1:n){
  paste0(d[i,])
  proton(action = "login", login="johnins", password= paste0(d[i,]))
}

employees[employees$surname=="Pietraszko",3] 

logs[login=="slap",-3] %>% group_by(host)%>% count 

proton(action = "server", host="194.29.178.16")

gsub(" .*$","",bash_history)%>% table
proton(action = "login", login="slap", password="DHbb7QXppuHnaXGN")

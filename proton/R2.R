install.packages("BetaBit")
library("BetaBit")
proton()

dane <-data.frame(employees)
head(dane)
login <-dane[c(dane$name =="John",dane$surname=="Insecure"),]

proton(action = "login", login="johnins")

hasla<-data.frame(top1000passwords)


for(i in 1:1000){
z <-proton(action = "login", login="johnins", password=top1000passwords[i])
if(z == "Success! User is logged in!"){haslo=top1000passwords[i] break}
}

proton(action = "login", login="johnins", password=haslo)


pietraszko.login <- employees$login[employees$name=="Slawomir" & employees$surname=="Pietraszko"]

log.slap <- logs[logs$login==pietraszko.login,]


most.logs <- as.data.frame(table(log.slap$host))

host.IP <- most.logs$Var1[which.max(most.logs$Freq)]

proton(action = "server", host=as.character(host.IP) )

bash_history

proton(action = "login", login = pietraszko.login, password = "pwd" )

bash_extracted <- gsub( " .*$", "",bash_history) 

commands <- table(bash_extracted)

proton(action = "login", login = pietraszko.login, password = "DHbb7QXppuHnaXGN" )

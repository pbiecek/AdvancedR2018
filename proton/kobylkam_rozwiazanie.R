install.packages("BetaBit")
library("BetaBit")


#Start

proton()

#Problem 1 - Login John Insecure 

john.login <- employees$login[employees$name=="John" & employees$surname=="Insecure"]

proton(action = "login", login = john.login)

#Problem 2 - Haslo John Insecure

top1000passwords

for (i in 1:length(top1000passwords)) {
z <- proton(action = "login", login = john.login, password = top1000passwords[i] )
if (z=="Success! User is logged in!") { password <- top1000passwords[i]
                                           break  } }

password

proton(action = "login", login = john.login, password = password )

#Problem 3 - Serwer, na ktory Pietraszko wchodzi najczesciej

View(logs)

pietraszko.login <- employees$login[employees$name=="Slawomir" & employees$surname=="Pietraszko"]

log.slap <- logs[logs$login==pietraszko.login,]
View(log.slap)

most.logs <- as.data.frame(table(log.slap$host))

host.IP <- most.logs$Var1[which.max(most.logs$Freq)]

proton(action = "server", host=as.character(host.IP) )

#Problem 4 - Haslo Pietraszko

bash_history

proton(action = "login", login = pietraszko.login, password = "pwd" )

bash_extracted <- gsub( " .*$", "",bash_history) 

commands <- table(bash_extracted)

#  "DHbb7QXppuHnaXGN" wystepuje tylko raz - to moze byc haslo

proton(action = "login", login = pietraszko.login, password = "DHbb7QXppuHnaXGN" )






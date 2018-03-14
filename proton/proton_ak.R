install.packages("BetaBit")
library("BetaBit")
proton()

employees
employees[employees$name=="John" & employees$surname=="Insecure","login"]
"johnins"
pas <- top1000passwords
head(top1000passwords)

for(i in 1:length(top1000passwords)){
  odp <- proton(action="login", login="johnins", password=top1000passwords[i])
}

logi <- logs
employees[employees$surname=="Pietraszko","login"]
#slap
logi_j <- logi[logi$login=="slap",]
logi_j_t <- logi[logi$login=="slap", "host"]
nr <- which.max(as.data.frame(table(logi_j_t))$Freq)
logi_j_t[nr]
#194.29.178.16
proton(action="server", host="194.29.178.16")



head(bash_history)
hist <- bash_history


pass <- character(length(bash_history))
for(i in 1:length(bash_history)){
pass[i]<- sub(" .*", "", bash_history[i])
}


pass1 <- unique(pass)

for(i in 1:length(pass1)){
  odp <- proton(action="login", login="slap", password=pass1[i])
}


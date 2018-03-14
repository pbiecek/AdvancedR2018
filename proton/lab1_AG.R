
install.packages('BetaBit')
library("BetaBit")
proton()

#
employees[employees$name=='John' & employees$surname=='Insecure',]
proton(action = "login", login="johnins")

#
top1000passwords
for(i in 1:length(top1000passwords)) {
  proton(action = "login", login="johnins", password=top1000passwords[i])
}

#
employees[employees$name=='Slawomir' & employees$surname=='Pietraszko',]
sp <- logs[logs$login=='slap',"host"]
hosty <- unique(logs[logs$login=='slap',"host"])
ile_razy <- numeric(length(hosty))
for (i in 1:length(ile_razy)) {
  ile_razy[i] <- length(sp[sp==hosty[i]])
}
hosty[ile_razy==max(ile_razy)]
proton(action = "server", host="194.29.178.16")

#
bash_history
for (i in 1:length(bash_history)) {
    for (j in 1:nchar(bash_history[i])) {
      if(substr(bash_history[i],j,j)==' ') {
        proton(action = "login", login="slap", password=substr(bash_history[i],1,j-1))
        break
      }
    }
}








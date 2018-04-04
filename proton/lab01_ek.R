library("BetaBit")
proton()

employees[which(employees[,1]=="John"),]
proton(action = "login", login="johnins")

top1000passwords

for(i in 1:250) {
  print(i)
  print(proton(action = "login", login="johnins", password=top1000passwords[i]))
}

# 120
top1000passwords[120]
proton(action = "login", login="johnins", password="q1w2e3r4t5")

employees[which(employees[,1]=="Slawomir"),]
table(as.character(logs[which(logs[,1]=="slap"),]$host))
proton(action = "server", host="194.29.178.16")

unique(unlist(lapply(1:length(bash_history), function(i) strsplit(bash_history[i],' ')[[1]][1])))
proton(action = "login", login="slap", password="DHbb7QXppuHnaXGN")

login<-employees[employees$name=="John" & employees$surname=="Insecure",3] 

proton(action = "login", login=login)


for (i in 1:length(top1000passwords))
{
  proton(action = "login", login=login, password=top1000passwords[i])
}

employees[employees$name=="Slawomir" & employees$surname=="Pietraszko",3] 
hostXD<-as.vector(unique(logs[logs$login=="slap",]$host))

proton(action = "server", host=hostXD[1])


head(bash_history)

proton(action = "login", login="slap", password="vi")

s<-gsub(" .*","",bash_history)
s2<-unique(s)

for (i in 1:length(unique(s2)))
{
  proton(action = "login", login="slap", password=s2[i])
}
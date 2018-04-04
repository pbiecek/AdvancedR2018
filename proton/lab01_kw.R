library(dplyr)
library("BetaBit")
library(stringr)

proton()
head(employees)
# When you finally find out what John's login is, use `proton(action = "login", login="XYZ")` command, where XYZ is Insecure's login.
login <- employees %>% 
  filter(name=='John', surname=='Insecure') %>% 
  select(login)

proton(action = "login", login=login)
# 
# Problem 2: Find John Insecure's password.
# 
# Use `proton(action = "login", login="XYZ", password="ABC")` command in order to log into the Proton server with the given credentials.
head(top1000passwords)
dim(top1000passwords)

s <- sapply(top1000passwords, function(x) proton(action = "login", login=login, password=x))




top1000passwords[which(s=='Success! User is logged in!')]
# 
# Problem 3: Check from which server Pietraszko logs into the Proton server most often.
# 
# Use `proton(action = "server", host="XYZ")` command in order to learn more  about what can be found on the XYZ server.
# The biggest chance to find something interesting is to find a server from which Pietraszko logs in the most often.

head(logs)

login_pietraszko <- employees %>% 
  filter(name=='Slawomir', surname=='Pietraszko') %>% 
  select(login)



most_freq_host <- logs %>% 
  filter(login=='slap') %>% 
  group_by(host) %>% 
  summarise(suma_logowan=n()) %>% 
  arrange(desc(suma_logowan)) %>% 
  head(1) %>% 
  select(host)

proton(action = "server", host=unfactor(most_freq_host))
# 
# 
# Problem 4: Find the Pietraszko's password.
# 
# In the `bash_history` dataset you will find all commands and parameters which have ever been entered.
# Try to extract from this dataset only commands (only strings before space) and check whether one of them looks like a password.
# Once you find the passord, use it to login into the proton server.

head(bash_history,20)
 
try_passwords <- sapply(bash_history, function(x) unlist(strsplit(x, split=" "))[1])
length(try_passwords)
unique(try_passwords)

s_pietraszko <- sapply(try_passwords, function(x) proton(action = "login", login='slap', password=x))


try_passwords[which(s_pietraszko =='Success! User is logged in!')]

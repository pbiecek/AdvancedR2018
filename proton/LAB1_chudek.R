install.packages("BetaBit")
library("BetaBit")
proton()

library('dplyr')
library("stringr")

# zad 0 
head(employees)

employees %>%
  filter(name == 'John' & surname == "Insecure" )

proton(action = "login", login="johnins")

top1000passwords

#top1000passwords[ !is.na(str_match(top1000passwords, '')) ] 
#proton(action = "login", login="XYZ", password="Insecure")


# zad 1 
n <- length(top1000passwords)

for (i in 1:n ){
    
   proton(action = "login", login="johnins", password = top1000passwords[i])
  }

#zad 2 

employees %>%
  filter(surname == "Pietraszko" )

head(logs)

logs %>%
  filter(login == "slap") %>%
  group_by(host) %>%
  count()

proton(action = "server", host = "194.29.178.16")

# zad 3 
#In the `bash_history` dataset you will find all commands and parameters which have ever been entered.
#Try to extract from this dataset only commands (only strings before space) and check whether one of them looks like a password.
#Once you find the passord, use it to login into the proton server.

bash_history
a <- gsub(" .*$", "", bash_history)
unique(a)

proton(action = "login", login="slap", password="DHbb7QXppuHnaXGN")


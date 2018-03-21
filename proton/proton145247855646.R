library("BetaBit")
proton()

#1
employees[employees$name=="John",]

proton(action = "login", login="johnins")

#2
n <- length(top1000passwords)
for (i in 1:n){
  proton(action = "login", login="johnins", password=top1000passwords[i])
}

#3

# Well done! This is the right password!
#   Bit used John Insecure's account in order to log into the Proton server.
# It turns out that John has access to server logs.
# Now, Bit wants to check from which workstation Pietraszko is frequently logging into the Proton server. Bit hopes that there will be some useful data.  
# 
# Logs are in the `logs` dataset. 
# Consecutive columns contain information such as: who, when and from which computer logged into Proton.
# 
# Problem 3: Check from which server Pietraszko logs into the Proton server most often.
# 
# Use `proton(action = "server", host="XYZ")` command in order to learn more  about what can be found on the XYZ server.
# The biggest chance to find something interesting is to find a server from which Pietraszko logs in the most often.





head(logs)
head(employees)
employees[employees$surname=="Pietraszko",]

pietr <- logs[logs$login=="slap",]

hosty <- unique(pietr$host)
length(hosty) #5
t.log <- c(nrow(pietr[pietr$host==hosty[1],]), nrow(pietr[pietr$host==hosty[2],]),nrow(pietr[pietr$host==hosty[3],]),nrow(pietr[pietr$host==hosty[4],]),nrow(pietr[pietr$host==hosty[5],]))
which.max(t.log)
hosty[1]
proton(action = "server", host="194.29.178.16")

#4


# It turns out that Pietraszko often uses the public workstation 194.29.178.16.
# What a carelessness.
# 
# Bit infiltrated this workstation easily. He downloaded `bash_history` file which contains a list of all commands that were entered into the server's console.
# The chances are that some time ago Pietraszko typed a password into the console by mistake thinking that he was logging into the Proton server.
# 
# Problem 4: Find the Pietraszko's password.
# 
# In the `bash_history` dataset you will find all commands and parameters which have ever been entered.
# Try to extract from this dataset only commands (only strings before space) and check whether one of them looks like a password.
# Once you find the passord, use it to login into the proton server.
# 


head(bash_history)
length(bash_history)
stringi::stri_extract_all_regex()




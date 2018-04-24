#GIT_anal
options(stringsAsFactors = FALSE)
#library(plyr)
library(stringi)
library(ggplot2)
library(dplyr)
#library(tidyr)
library(wordcloud)
#library(plotly)
library(data.table)



load("DB_GIT")

user <- 1
repo <- 1
script <- 1
x <- ob[[user]]
x <- x[[repo]]
x <- x[[script]]
x
db_git <- stri_list2matrix(x, byrow=FALSE)
db_git <- data.frame(db_git)
colnames(db_git) <- c("libraries", "f_names", "v_names", "user_name", "repo_name", "file_name")
db_git$user_name <- db_git$user_name[1]
db_git$repo_name <- db_git$repo_name[1]
db_git$file_name <- db_git$file_name[1]

n <- length(ob)
for(user in 2:n){
  
  m <- length(ob[[user]])
  for(repo in 1:m){
    
    k <- length(ob[[user]][[repo]])
    for(file in 1:k){
      
      x <- ob[[user]][[repo]][[file]]
      if(length(x)<6){
        x <- list("0", "0", "0", "0", "0", "0")
      }
      y <- stri_list2matrix(x, byrow=FALSE)
      y <- data.frame(y)
      colnames(y) <- c("libraries", "f_names", "v_names", "user_name", "repo_name", "file_name")
      y$user_name <- y$user_name[1]
      y$repo_name <- y$repo_name[1]
      y$file_name <- y$file_name[1]
      
      db_git <- rbind(db_git, y)
    }
    
  }
}

save(db_git, file="DBB_GIT")

#################################
#################################
#################################
load("DBB_GIT")

########################
# how many repos has author
########################
df <- db_git %>%
  select(user_name, repo_name) %>%
  group_by(user_name) %>%
  summarise(n_repo=n_distinct(repo_name))

ggplot(df, aes(x=n_repo)) +
  geom_bar() +
  ggtitle("Histogram licznosci repozytoriow per autor - GIT")

ggsave("plots/git_n_repo.pdf")

########################
# filenames
########################
df <- db_git %>%
  select(user_name, repo_name, file_name) %>%
  distinct() %>%
  group_by(file_name) %>%
  summarise(n=n()) %>%
  filter(n>2) %>%
  arrange(n) %>%
  top_n(20)

df$file_name <- factor(df$file_name, levels=df$file_name[order(df$n)])

ggplot(df, aes(x=file_name, y=n)) +
  geom_bar(stat="identity") + 
  coord_flip() +
  ggtitle("Najczestsze nazwy plikow - GIT")
ggsave("plots/git_filenames.pdf")

#wordcloud(df$file_name, df$n, min.freq = 10)

#########
df <- db_git %>%
  select(user_name, repo_name, file_name) %>%
  distinct()
df$file_name_length <- nchar(df$file_name)

ggplot(df, aes(x=file_name_length)) +
  geom_bar() +
  ggtitle("Rozklad dlugosci nazw plikow - GIT")

ggsave("plots/git_filelength.pdf")

########################
# variable names
########################

df <- db_git %>%
  select(user_name, repo_name, file_name, v_names) %>%
  distinct() %>%
  na.omit()

df2 <- df %>%
  group_by(v_names) %>%
  summarise(n=n()) %>%
  arrange(desc(n)) %>%
  filter(n>=50) %>%
  top_n(20)

df2$v_names <- factor(df2$v_names, levels=df2$v_names[order(df2$n)])
ggplot(df2, aes(x=v_names, y=n)) +
  geom_bar(stat="identity") + 
  coord_flip() +
  ggtitle("Najczestsze nazwy zmiennych - GIT")
ggsave("plots/git_varname.pdf")


countCharOccurrences <- function(char, s) {
  chr.pos <- which(unlist(strsplit(s,NULL)) == char) 
  chr.count <- length(chr.pos)
  return(chr.count)
}

w1 <- w2 <- numeric(0)
for(i in 1:nrow(df)){
  n1 <- countCharOccurrences(".", df$v_names[i])
  n2 <- countCharOccurrences("_", df$v_names[i])
  w1 <- c(w1,n1)
  w2 <- c(w2,n2)
}
w3 <- sapply(regmatches(df$v_names, gregexpr("[A-Z]", df$v_names, perl=TRUE)), length)

df$n_dot <- w1
df$n_underscore <- w2
df$n_uppercase <- w3
df$v_length <- nchar(df$v_names)

ggplot(df, aes(x=v_length)) + 
  geom_bar() +
  ggtitle("Rozklad dlugosci nazw zmiennych - GIT")
ggsave("plots/git_varlength.pdf")


df$n_dot <- ifelse(df$n_dot>=1, 1, 0)
df$n_underscore <- ifelse(df$n_underscore>=1, 1, 0)
df$n_uppercase <- ifelse(df$n_uppercase>=1, 1, 0)

df2 <- df %>% 
  select(n_dot, n_underscore, n_uppercase) %>%
  colSums()

df2 <- add_rownames(melt(df2), var="sign")
df2 <- data.frame(df2)
ggplot(df2, aes(x=sign, y=value, fill=sign))+
  geom_bar(width = 1, stat = "identity") +
  ggtitle("Czestosc stosowania konwencji nazywania zmiennych - GIT")
ggsave("plots/git_var_signs.pdf")

########################
# function names
########################

df <- db_git %>%
  select(user_name, repo_name, file_name, f_names) %>%
  distinct() %>%
  na.omit()

countCharOccurrences <- function(char, s) {
  chr.pos <- which(unlist(strsplit(s,NULL)) == char) 
  chr.count <- length(chr.pos)
  return(chr.count)
}

w1 <- w2 <- numeric(0)
for(i in 1:nrow(df)){
  n1 <- countCharOccurrences(".", df$f_names[i])
  n2 <- countCharOccurrences("_", df$f_names[i])
  w1 <- c(w1,n1)
  w2 <- c(w2,n2)
}
w3 <- sapply(regmatches(df$f_names, gregexpr("[A-Z]", df$f_names, perl=TRUE)), length)

df$n_dot <- w1
df$n_underscore <- w2
df$n_uppercase <- w3
df$f_length <- nchar(df$f_names)

ggplot(df, aes(x=f_length)) + 
  geom_bar() +
  ggtitle("Rozklad dlugosci nazw funkcji - GIT")
ggsave("plots/git_funlength.pdf")

df$n_dot <- ifelse(df$n_dot>=1, 1, 0)
df$n_underscore <- ifelse(df$n_underscore>=1, 1, 0)
df$n_uppercase <- ifelse(df$n_uppercase>=1, 1, 0)

df2 <- df %>% 
  select(n_dot, n_underscore, n_uppercase) %>%
  colSums()

df2 <- add_rownames(melt(df2), var="sign")
df2 <- data.frame(df2)
ggplot(df2, aes(x=sign, y=value, fill=sign))+
  geom_bar(width = 1, stat = "identity") +
  ggtitle("Czestosc stosowania konwencji nazywania funkcji - GIT")
ggsave("plots/git_fun_signs.pdf")


############################
# ile skryptów R
df <- db_git %>%
  select(user_name, repo_name, file_name) %>%
  distinct()
nrow(df)

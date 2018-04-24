#CRAN_anal
# [[1]] bibliteka
# [[1]][[1]] pierwszy skrypt w bibliotece
# [[1]][[1]]$f_names nazwy funkcji
#laczyc po descrption bo tam nazwa bilioteki
# brac unikalne ze wzgledu na plik
options(stringsAsFactors = FALSE)
#library(plyr)
library(stringi)
library(ggplot2)
library(dplyr)
#library(tidyr)
library(data.table)

# x <- db$FUNCTIONS
# y <- x[[1]]
# y <- y[[2]]
# table(y$v_names)


#############################
# descriptions
#############################
load("DB_CRAN_WHOLE")
n <- length(db$DESCRIPTION)
x <- db$DESCRIPTION
x <- x[[1]]

y <- stri_list2matrix(x, byrow=FALSE)
y <- data.frame(y)
colnames(y) <- c("authors", "licence", "imp_packages", "libr_name")
y$libr_name <- y$libr_name[1]
db_desc <- y
for (i in 2:n) {
  x <- db$DESCRIPTION[[i]]
  y <- stri_list2matrix(x, byrow=FALSE)
  y <- data.frame(y)
  colnames(y) <- c("authors", "licence", "imp_packages", "libr_name")
  y$libr_name <- y$libr_name[1]
  db_desc <- rbind(db_desc, y)
}

save(db_desc, file="DBB_DESC")

load("DBB_DESC")

############
# rozklad licencji
df <- db_desc %>%
  group_by(licence) %>%
  summarise(n=n()) %>%
  na.omit() %>%
  filter(n>5) %>%
  top_n(15)

df$licence <- factor(df$licence, levels=df$licence[order(df$n)])
ggplot(df, aes(x=licence, y=n)) +
  geom_bar(stat="identity") + 
  coord_flip() + 
  ggtitle("Najczesciej uzywane licencje - CRAN")
ggsave("plots/cran_licence.pdf")

############
# ilu autorow ma pakiet
df <- db_desc %>%
  select(authors, libr_name) %>%
  na.omit() %>%
  distinct() %>%
  group_by(libr_name) %>%
  summarise(n=n())

df1 <- df %>%
  filter(n<10)

ggplot(df1, aes(x=n)) +
  geom_bar() +
  ggtitle("Histogram liczby autorow per pakiet - CRAN")
ggsave("plots/cran_authors.pdf")

df <- df %>%
  filter(n>10) %>%
  top_n(20)

df$libr_name <- factor(df$libr_name, levels=df$libr_name[order(df$n)])
ggplot(df, aes(x=libr_name, y=n)) +
  geom_bar(stat="identity") + 
  coord_flip() +
  ggtitle("Pakiety o najwiekszej liczbie autorow - CRAN")
ggsave("plots/cran_most_authors.pdf")

############
# z jakich bibliotek korzysta pakiet
df <- db_desc %>%
  select(libr_name, imp_packages) %>%
  na.omit()

df <- df %>%
  group_by(imp_packages) %>%
  summarise(n=n()) %>%
  filter(n>50) %>%
  arrange(-n) %>%
  top_n(20)

df$imp_packages <- factor(df$imp_packages, levels=df$imp_packages[order(df$n)])
ggplot(df, aes(x=imp_packages, y=n)) +
  geom_bar(stat="identity") + 
  coord_flip()+
  ggtitle("Najczesciej stosowane pakiety - CRAN")
ggsave("plots/cran_packages.pdf")

##
df <- db_desc %>%
  select(libr_name, imp_packages) %>%
  na.omit() %>%
  filter(imp_packages != "No imported packages") %>%
  group_by(libr_name) %>%
  summarise(n=n()) 

ggplot(df, aes(x=n)) +
  geom_bar()+
  ggtitle("Histogram licznosci uzytych pakietów per biblioteka - CRAN")
ggsave("plots/cran_count_packages.pdf")

#############################
# functions and variables
#############################
y <- db$FUNCTIONS
n <- length(db$DESCRIPTION)
x <- db$DESCRIPTION

bib <- 1
bib_name <- db$DESCRIPTION[[bib]]$libr_name
y <- db$FUNCTIONS[[bib]]
script <- 1
y[[script]]
if(length(y[[script]])==2){
  y[[script]]$libraries <- "0"
  y[[script]] <- y[[script]][c("libraries", "f_names", "v_names")]
}
db_fun <- stri_list2matrix(y[[script]], byrow=FALSE)
db_fun <- data.frame(db_fun)
colnames(db_fun) <- c("libraries", "f_names", "v_names")
db_fun$id_script <- script
db_fun$libr_name <- bib_name

for (bib in 2:n) {
  
  bib_name <- db$DESCRIPTION[[bib]]$libr_name
  y <- db$FUNCTIONS[[bib]]
  m <- length(y)
  
  for(script in 1:m){
    
    if(length(y[[script]])==2){
      y[[script]]$libraries <- "0"
      y[[script]] <- y[[script]][c("libraries", "f_names", "v_names")]
    }
    
    tmp <- stri_list2matrix(y[[script]], byrow=FALSE)
    tmp <- data.frame(tmp)
    colnames(tmp) <- c("libraries", "f_names", "v_names")
    tmp$id_script <- script
    tmp$libr_name <- bib_name
    
    db_fun <- rbind(db_fun, tmp)
  }
  
}

save(db_fun, file="DBB_FUN")

############################
# analiza funkcji
############################
load("DBB_FUN")
#kolumna libraries nic nie ma

df <- db_fun %>%
  select(libr_name, id_script, f_names) %>%
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
  geom_bar()+
  ggtitle("Rozklad dlugosci nazw funkcji - CRAN")
ggsave("plots/cran_funlength.pdf")


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
  ggtitle("Czestosc stosowania konwencji nazywania funkcji - CRAN")
ggsave("plots/cran_fun_signs.pdf")

############################
# analiza zmiennych
############################
df <- db_fun %>%
  select(libr_name, id_script, v_names) %>%
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
  coord_flip()+
  ggtitle("Najczestsze nazwy zmiennych - CRAN")
ggsave("plots/cran_varname.pdf")

countCharOccurrences <- function(char, s) {
  chr.pos <- which(unlist(strsplit(s,NULL)) == char) 
  chr.count <- length(chr.pos)
  return(chr.count)
}
#!!!!!!!!!!!!!!!!!!!
#!!!!!!!!!!!!!!!!!!
#dluga petla
w1 <- w2 <- numeric(0)
for(i in 1:nrow(df)){ #jak dasz rade to zrob te petle bardziej optymalnie
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
  geom_bar()+
  ggtitle("Rozklad dlugosci nazw zmiennych - CRAN")
ggsave("plots/cran_varlength.pdf")


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
  ggtitle("Czestosc stosowania konwencji nazywania zmiennych - CRAN")
ggsave("plots/cran_var_signs.pdf")

############################
# ile skryptów R
df <- db_fun %>%
  group_by(libr_name) %>%
  summarise(m=max(id_script))
sum(df$m)

# Mateusz Kobyłka
# Dopasowanie rozkładów do danych (cenzurowanych) przy pomocy pakietu fitdistrplus

# Ładujemy pakiet fitdistrplus

library(fitdistrplus)

# Losujemy 500 obserwacji z rozkladu N(0,1)

set.seed(1234)
x <- rnorm(500,0,1)

# Dopasowujemy rozkład normalny do naszych danych
f <- fitdist(x,distr="norm")
summary(f)


#Rysujemy gęstość dopasowaną i prawdziwą
pts <- seq(-3,3,length.out=500)
plot(dnorm(pts,0,1), type = "l", xlab = "x", ylab="y",xaxt="n")
axis(side=1, at=250,labels=0)
lines(dnorm(pts,f$estimate[1],f$estimate[2]),col="red", type="l")
legend("topright",legend=c("N(0,1)","fit"), text.col=c("black","red"))


#Dane cenzurowane

xcens <- data.frame(left = x, right = x)
head(xcens)
plotdistcens(xcens, Turnbull = FALSE)

# Funkcja cenzurująca

cens <- function (a, type) 
{
  
  if(type=="left") {
    
   a$left <- NA
   a$right <- ifelse(a$right>=0, (a$right)*(1/runif(1,0,1)), (a$right)*runif(1,0,1))
     
  }
  if (type == "right") {
    
    a$left <- ifelse(a$left>=0, a$left*runif(1,0,1), a$left*(1/runif(1,0,1)))
    a$right <- NA
    
  }
  a
}


# Cenzurujemy x% obserwacji, x/2 % lewostronnie, x/ 2 % prawostronnie

u <- sample(1:500,100) #20%
v <- sample(1:500,200) #40%
w <- sample(1:500,400) #80%

xcens20 <- xcens
xcens40 <- xcens
xcens80 <- xcens


xcens20[u[1:50],] <- cens(xcens20[u[1:50],],type="right") 
xcens20[u[51:100],] <- cens(xcens20[u[51:100],],type="left") 

xcens40[v[1:100],] <- cens(xcens40[v[1:100],],type="right") 
xcens40[v[101:200],] <- cens(xcens40[v[101:200],],type="left") 

xcens80[w[1:200],] <- cens(xcens80[w[1:200],],type="right") 
xcens80[w[201:400],] <- cens(xcens80[w[201:400],],type="left") 

# Wykres danych cenzurowanych
plotdistcens(xcens20, Turnbull = FALSE)
plotdistcens(xcens40, Turnbull = FALSE)
plotdistcens(xcens80, Turnbull = FALSE)


fcens20 <- fitdistcens(xcens20,distr="norm") 
fcens40 <- fitdistcens(xcens40,distr="norm")
fcens80 <- fitdistcens(xcens80,distr="norm")

#Wykres gęstości prawdziwej i gęstości wyestymowanych z danych

plot(dnorm(pts,0,1),type = "l", xlab = "x", ylab="y",xaxt="n")
axis(side=1, at=250,labels=0)
lines(dnorm(pts,fcens20$estimate[1],fcens20$estimate[2]),col="red", type="l")
lines(dnorm(pts,fcens40$estimate[1],fcens40$estimate[2]),col="green", type="l")
lines(dnorm(pts,fcens80$estimate[1],fcens80$estimate[2]),col="blue", type="l")
legend("topright",legend=c("N(0,1)","20%","40%","80%"), text.col=c("black","red","green","blue"))

summary(fcens20)
summary(fcens40)
summary(fcens80)

install.packages("pvclass")
library(pvclass)

###Zbior danych w pakiecie

data("buerk")
?buerk
head(buerk)

### przygotowanie danych

liczby<-sample(1:150,30)
X.train<-iris[-liczby,1:4]
Y.train<-iris[-liczby,5]
X.test<-iris[liczby,1:4]
Y.test<-iris[liczby,5]

### Glowne funkcje

?pvs
?cvpvs

?pvs.gaussian
?pvs.knn
?pvs.wnn
?pvs.logreg

### Przyklady
## gausik
pv.g<-pvs.gaussian(X.test,X.train,Y.train,cova = "standard")

# wizualizacja
?analyze.pvs
analyze.pvs(pv.g)
analyze.pvs(pv.g,Y = Y.test)  

## knn

pv.k<-pvs.knn(X.test,X.train,Y.train,k=10,distance = "e",cova = "standard")
analyze.pvs(pv.k,Y = Y.test)

## wnn

pv.w<-pvs.wnn(X.test,X.train,Y.train,wtype="linear",tau=0.2,distance = "e",cova="standard")
analyze.pvs(pv.w,Y = Y.test)

##  logreg

pv.l<-pvs.logreg(X.test,X.train,Y.train,pen.method = "none")
analyze.pvs(pv.l,Y = Y.test)


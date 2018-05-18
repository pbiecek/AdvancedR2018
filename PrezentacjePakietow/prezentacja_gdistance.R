#install.packages("gdistance")
library("gdistance")
set.seed(123)

#tworzymy raster
r <- raster(ncol = 3, nrow = 3)
r[] <- 1:ncell(r)
r
plot(r)
text(r)

#macierz przejścia
tr1 <- transition(r, transitionFunction = mean, directions = 8)
tr1

#niesymetryczna macierz przejścia
r[] <- runif(9)
ncf <- function(x) max(x) - x[1] + x[2]
tr2 <- transition(r, ncf, 4, symm = FALSE)
tr2

#różne możliwości
tr3 <- tr1 * tr2
tr3 <- tr1 + tr2
tr3 <- tr1 * 3
tr3 <- sqrt(tr1)
tr3[cbind(1:9, 1:9)] <- tr2[cbind(1:9, 1:9)]
tr3[1:9, 1:9] <- tr2[1:9, 1:9]

#wyświtlmy jak to wygląda
tr3[1:5, 1:5]
image(transitionMatrix(tr1))


plot(raster(tr3), main = "raster(tr3)", xlab = "Longitude (degrees)",
     ylab = "Latitude (degrees)")

#korekta wartości macierzy przejścia
tr2C <- geoCorrection(tr2, type = "c")
tr3C <- geoCorrection(tr3, type = "c", multpl = FALSE, scl = TRUE)

#coś tam żeby się mniej liczyło chyba
CorrMatrix <- geoCorrection(tr3, type = "r", multpl = TRUE, scl = TRUE)
tr3R <- tr3 * CorrMatrix

#obliczanie odległości
sP <- cbind(c(-100, -100, 100), c(50, -50, 50))
costDistance(tr3C, sP)
commuteDistance(tr3R, sP)
rSPDistance(tr3R, sP, sP, theta = 1e-12, totalNet = "total")

#rozchodzenie się ścieżek
origin <- SpatialPoints(cbind(0, 0))
rSPraster <- passage(tr3C, origin, sP[1,], theta = 3)
plot(rSPraster)


#Ścieżki nachodzące
r1 <- passage(tr3C, origin, sP[1,], theta = 1)
r2 <- passage(tr3C, origin, sP[2,], theta = 1)
rJoint <- min(r1, r2)
rDiv <- max(max(r1, r2) * (1 - min(r1, r2)) - min(r1, r2), 0)
plot(rJoint)
text(-100, 50, labels="sp1")
text(-100, -50, labels="sp2")
text(10, 0, labels="O")


#Przykład: Wycieczka górska
r <- raster(system.file("external/maungawhau.grd", package = "gdistance"))

#nachylenie-slope
altDiff <- function(x) x[2] - x[1]
hd <- transition(r, altDiff, 8, symm= FALSE)
slope <- geoCorrection(hd)

#prędkość dla połączonych komórek
adj <- adjacent(r, cells = 1:ncell(r), pairs = TRUE, directions = 8)
speed <- slope
speed[adj] <- 6 * exp(-3.5 * abs(slope[adj] + 0.05)) #wzór na prędkośc jest dany explicite

#ostateczne "koszty"
Conductance <- geoCorrection(speed)

#Dla pewnych punktóW A i B znajdujemy najkrótszą ścieżkę
A <- c(2667670, 6479000)
B <- c(2667800, 6479400)
AtoB <- shortestPath(Conductance, A, B, output = "SpatialLines")
BtoA <- shortestPath(Conductance, B, A, output = "SpatialLines")

#zobrazowanie
plot(r, xlab = "x coordinate (m)", ylab = "y coordinate (m)",
         legend.lab = "Altitude (masl)")
lines(AtoB, col = "red", lwd = 2)
lines(BtoA, col = "blue")
text(A[1] - 10, A[2] - 10, "A")
text(B[1] + 10, B[2] + 10, "B")


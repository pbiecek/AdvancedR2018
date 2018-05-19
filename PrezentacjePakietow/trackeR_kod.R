
#install.packages("trackeR")
library("trackeR")
data("runs", package = "trackeR")
class(runs)
runs[1]
# ----------------------------------------

summary(runs, session = 1:2,  movingThreshold = 1)

#----------------------------------------

plot(runs, session = 27, what = c("altitude", "pace"))

plotRoute(runs, session = 4, zoom = 13)

leafletRoute(runs, session = 8:13)  # mapa interaktywna

runSummaryFull <- summary(runs)
plot(runSummaryFull, group = c("total", "moving"),
     what = c("avgSpeed", "distance", "duration", "avgHeartRate"))

timeline(runSummaryFull)


#------------------------------------

runZones <- zones(runs[1:4], what = "speed", breaks = list(speed = c(0, 2:6, 12.5)))
plot(runZones)


#------------------------------------

wexp <- Wprime(runs, session = 11, quantity = "expended",
               cp = 4, version = "2012")
plot(wexp, scaled = TRUE)

#------------------------------------

wexp <- Wprime(runs, session = 11, quantity = "expended",
               cp = 4, version = "2012")
plot(wexp, scaled = TRUE)

#------------------------------------

dProfile <- distributionProfile(runs, session = 1:4,
                                what = c("speed", "heart.rate"),
                                grid = list(speed = seq(0, 12.5, by = 0.05), heart.rate = seq(0, 250)))
plot(dProfile, multiple = TRUE)

#------------------------------------

cProfile <- concentrationProfile(dProfile, what = "speed")
plot(cProfile, multiple = TRUE)

#------------------------------------

getUnits(run)

runTr2 <- changeUnits(runs, variable = "speed", unit = "mi_per_h")
getUnits(runTr2)

m_per_s2ft_per_h <- function(x) x * 3937/1200 * 3600
changeUnits(summary(runs, session = 1), variable = "speed", unit = "ft_per_h")

#------------------------------------

install.packages("gridExtra")
library(gridExtra)

plot1 <- plot(runs, session = 4, what = "speed", threshold = FALSE)
run4 <- threshold(runs[4], variable = "speed", lower = 0, upper = 12.5)
plot2 <- plot(run4, what = "speed", threshold = FALSE) +
  ggplot2::expand_limits(y = c(0, 21))

grid.arrange(plot1, plot2, ncol=2)

#------------------------------------

run4S_20 <- smoother(run4, what = "speed", fun = "median", width = 20)
plot3 <- plot(run4S_20, what = "speed", smooth = FALSE) +
  ggplot2::expand_limits(y = c(0, 12.5))

run4S_5 <- smoother(run4, what = "speed", fun = "median", width = 5)
plot4 <- plot(run4S_5, what = "speed", smooth = FALSE) +
  ggplot2::expand_limits(y = c(0, 12.5))

grid.arrange(plot3, plot4, ncol=2)

#------------------------------------








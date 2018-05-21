# Symulacja

#install.packages("epinet")
library("epinet")

# Symulacja

# Budujemy macierz danych przestrzennych dla 50 osob
set.seed(1)
N <- 50
mycov <- data.frame(id = 1:N, xpos = runif(N), ypos = runif(N))
dyadCov <- BuildX(mycov, binaryCol = list(c(2, 3)), binaryFunc = "euclidean")

# budujemy siec
eta <- c(0, -7) # jak kolejne wspolczynniki wplywaja na prawdopodobienstwo istnienia polaczenia
net <- SimulateDyadicLinearERGM(N = N,
                                dyadiccovmat = dyadCov,
                                eta = eta)

# Symulacja epidemii
epi <- SEIR.simulator(M = net,
                      N = N,
                      beta = 1,
                      ki = 3,
                      thetai = 7,
                      ke = 3,
                      latencydist = "gamma")
epi

# Wykres drzewa transmisji
plot(epi, e.col = "slategrey", i.col = "red")

# algorytm  MCMC na symulowanych danych
mcmcinput <- MCMCcontrol(nsamp = 1000000, thinning = 100, etapropsd = c(1, 1))
priors <- priorcontrol(bprior = c(0, 4), tiprior = c(1, 15), teprior = c(1, 15), etaprior = c(0, 10, 0, 10), kiprior = c(1, 7), keprior = c(1, 7), priordists = "uniform")
out <- epinet(~ xpos.ypos.L2Dist, epidata = epi, dyadiccovmat = dyadCov, mcmcinput = mcmcinput, priors = priors)


# HISTOGRAM DLA ROZKŁADÓW A POSTERIORI PARAMETRÓW SIECI
hist(out$eta[, 1], main = "", xlab = "eta1")
abline(v = sort(out$eta[, 1])[length(out$eta[, 1]) * 0.025], col = "red")
abline(v = sort(out$eta[, 1])[length(out$eta[, 1]) * 0.975], col = "red")

hist(out$eta[, 2], main = "", xlab = "eta2")
abline(v = sort(out$eta[, 2])[length(out$eta[, 2]) * 0.025], col = "red")
abline(v = sort(out$eta[, 2])[length(out$eta[, 2]) * 0.975], col = "red")



# PRZYKŁAD2 : Epidemia Odry w Hagelloch

# Liczba nowych osob przenoszacych epidemie na przestrzeni czasu dla roznych klas
library("ggplot2")

# zbior do pobrania z https://www.jstatsoft.org/article/view/v083i11
load("SuppPlotData.RData")


indivdata$Classroom <- factor(indivdata$Classroom, 2:0)
cbPalette <- c("#56B4E9", "#E69F00", "#999999")

dataplot <- ggplot(indivdata, aes(Itime - min(Itime))) +
  geom_histogram(aes(fill = Classroom), binwidth = 1) +
  scale_x_continuous(name = "Czas(w dniach)") +
  scale_y_continuous(name = "Liczba nowych nowych zarazajacych jednostek",
                     limits = c(0, 21), expand = c(0, 0)) +
  scale_fill_manual(values = cbPalette, breaks = c("0", "1", "2"),
                    labels = c("none", "   1", "   2")) + theme_bw() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16)) +
  theme(legend.title = element_text(size = 14, face = "bold")) +
  theme(legend.text = element_text(size = 14))
dataplot

# Dane przestrzenne na wykresie

plot(spatdata$X, spatdata$Y,
     xlab = "Wspolrzedna X (w metrach)", ylab = "Wspolrzedna Y (w metrach)",
     cex.lab = 1.4, cex.axis = 1.5, pch = 19,
     xlim = c(0, 300), ylim = c(0, 250), cex = spatdata$Num/2.5)

# MCMC ALGORYTM
# kompilacja ponizszego kodu, ze wzgledu na ilosc iteracji moze zajac kilka godzin
mcmcinput <- MCMCcontrol(nsamp = 20000000, thinning = 1000, extrathinning = 10, burnin = 1000000, seed = 1, etapropsd = c(rep(0.05, times = 3), 0.005))
priors <- priorcontrol(etaprior = c(0, 3, 0, 3, 0, 3, 0, 3/40),  bprior = c(0, 4), tiprior = c(0.25, 0.75), teprior = c(0.25, 1), keprior = c(8, 20), kiprior = c(15, 25), priordists = "uniform")
out <- epinet(~ `Classroom 1` + `Classroom 2` + `House Distance`, epidata = HagellochTimes, dyadiccovmat = HagellochDyadCov, mcmcinput = mcmcinput, priors = priors)

summary(out)

# mozna dane wykowe zapisac do pliku
write.epinet(out, "HagellochOutput")
# Monika Chudek 
# Kamil Romaszko

library(devtools)
install_github("hugo4r/hugo")
library(hugo)


hugo_start_investigation()
# +1

dane <- hugo_get_object('pbiecek', 'AdvancedR2018', 'HR_data.csv')
# nie działa 


dane <- hugo_read_data("https://raw.githubusercontent.com/pbiecek/AdvancedR2018/master/Projekt2/HR_data.csv",
                       header = T, separator = "\t", decimal = ".") 
# nie domysla sie parametrów
# +1
head(dane)

hugo_summarise_data(dane)
# strasznie dużo pytań ;)
# +1


hugo_show_history()
# zapisuje funkcje skutkujace errory
# wypisuje w konsoli wywołanie do pierwszego przecinka
# +1


dane$left <- as.factor(dane$left)
models <- hugo_train_model(data = dane, formula = "left~.")
# nie zmienia wektora odpowiedzi na faktor 
# nie informuje o postępie 
# brak możliwości zmiany parametów modeli klasyfikacji 
# opisfunkcji -  brak values
# ??? brak katalogu
# +1

print(models)

pred <- predict(models, newdata = dane, type = "prob")


hugo_memorise_plot({
  plot(pred[,2], col=dane$left)
  abline(a=0.5, b=0)
})
# tak jest ok 


tab <- table(pred, dane$left)
acc <- sum(diag(tab))/ sum(tab)
acc
# Accuracy: 1 xD


hugo_memorise_model(models)
# +1

hugo_save_investigation()
# zapisuje zbiory danych == dublowanie danych 
# +1

#install.packages('getPass')
hugo_share_object(models)
# Error in 'git2r_clone'
# na linuxie działa 
# +1


hugo_show_statistics()
# plot = 0, mimo że jeden plot został zapisany 
# trainedModel = 0, model wytrenowany hugo_train_model i zapisany hugo_memorise_model


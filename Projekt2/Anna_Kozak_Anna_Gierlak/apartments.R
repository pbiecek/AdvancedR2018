#devtools::install_github("hugo4r/hugo")

library(hugo)
hugo_start_investigation()

dane <- hugo_read_data('H:/Windows7/Documents/hugo_investigation_2018_06_13/apartments.xlsx')
head(dane)
View(dane)

hugo_clean_data(dane)
dane[,6] <- as.factor(dane[,6]) #koniecznosc poprawy funckji: dla napisow niebedacych factorem nie dziala
hugo_clean_data(dane)

hugo_summarise_data(dane)
# Warning message:
#   In normalizePath(path.expand(path), winslash, mustWork) :
#   path[1]="./hugo_investigation_2018_06_13/gallery": The system cannot find the file specified
#ale raport zostal wygenerowany

hugo_train_model(dane, m2.price ~ .) # :(
model <- rpart::rpart(m2.price ~ ., dane)

hugo_memorise_model(model)
# Warning message:
#   In value[[3L]](cond) :
#   From package "broom" while creating a summary: simpleWarning in tidy.default(model):
#   No method for tidying an S3 object of class rpart , using as.data.frame

pred <- predict(model)
MSE <- mean((dane[,1]-pred)^2)
hugo_memorise(MSE)

predykcje <- data.frame(y = dane$m2.price, y_pred = pred)
#hugo_memorise_table(predykcje) #nie dziala :(

library(ggplot2)
wykres <- ggplot(aes(x = y, y = y_pred), data = predykcje) + geom_point()
hugo_memorise_plot(wykres)

res <- data.frame(x = 1:nrow(dane), y = dane[,1]-pred)
res_wykres <- ggplot() + geom_point(data = res, aes(x,y), colour = 'blue') +
  ylab('rezidua') + ggtitle('Wykres reszt dla dopasowanego modelu')
res_wykres

hugo_memorise_plot(res_wykres)

hugo_save_investigation()
rm(list = ls())
hugo_continue_investigation(session_name = 'session0')

hugo_show_statistics()

hugo_show_history()


hugo_share_object(MSE)

?hugo_share_object(MSE) #nie dziala na tutjeszym komputerze

hugo_share_secret(MSE, 'secret')
















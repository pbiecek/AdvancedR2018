devtools::install_github("hugo4r/hugo")
library(hugo)

hugo_start_investigation("Jablonska_Kobylka") #rozpoczecie badania

dane <- hugo_read_data(path="https://raw.githubusercontent.com/pbiecek/AdvancedR2018/master/Projekt2/HR_data.csv",
                       separator="", decimal = ".", header = TRUE) #wczytanie danych

dane$left <- as.factor(dane$left)
hugo_clean_data(dane) #uzupelnienie danych - brak NA

w <-c(sample(1:3000,size=50),sample(3001:1499,size=50))

train <- dane[w,]
test <- dane[-w,]

hugo_memorise(train) #zapisz zbiory treningowe i testowe
hugo_memorise(test)

model <- hugo_train_model(formula = "left ~ .", data = train) #trenowanie modelu

hugo_memorise_model(model = model, name = "HR_model")

levels <- predict(model, newdata = test)

tab <- table(test$left,levels) #tabela reklasyfikacji na zbiorze testowym

hugo_memorise_table(table = tab, name = "HR_model_tabela_reklasyfikacji") #zapisanie tabeli reklasyfikacji

err <- 1-sum(diag(tab))/sum(tab) #blad predykcji

hugo_memorise_plot(plot = plot(table(test$left,levels)),name="HR_porownanie_ryzyka_na_zbiorze_testowym")
hugo_show_statistics() #wyswietl statystyki

hugo_save_investigation(session_name="model_HR") #zapisz badanie

hugo_continue_investigation(session_name="model_HR") #wczytaj badanie ponownie - jest mozliwosc odtworzenia


hugo_show_history() #wyswietl historie

hugo_share_object(model) #zapisanie modelu na swoim githubie
hugo_share_secret(model, passphrase = "password") #zapisanie modelu z szyfrowaniem

hugo_show_history() #historia jeszcze raz

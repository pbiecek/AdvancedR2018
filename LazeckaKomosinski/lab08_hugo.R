#lab8 hugo
#Dariusz Komosinski, Malgorzata Lazecka

devtools::install_github("hugo4r/hugo")

library(hugo)

setwd("H:\\Windows7\\Documents\\R dla zaawansowanych")
#getwd()

hugo_start_investigation("LazeckaKomosinski") #dziala

db <- hugo_read_data("apartments.xlsx") #dziala
db$district <- as.factor(db$district)

db <- hugo_clean_data(db) #dziala

# tabelki nie zapiszemy ;<

hugo_summarise_data(db) #dziala

plot_to_save <- plot(db)

hugo_memorise_plot(plot_to_save) #dziala


hugo_train_model(data=db, formula=m2.price~.) #nie mozna zastosowac do regresji
model_lm <- lm(data=db, formula=m2.price~.)

hugo_memorise_model(model_lm) #dziala

#MSE
mse <- sum((db$m2.price - model_lm$fitted.values)^2) / nrow(db)
#zapiszmy mse
hugo_memorise(mse) #dziala


hugo_memorise_plot({plot(db$m2.price, model_lm$fitted.values)
  abline(0,1)}, name="fitted vs true") #dziala

hugo_memorise_plot(plot(model_lm$residuals), name="reszty")

hugo_show_history() #dziala

hugo_save_investigation() #dziala

#nie widzi funkcji:
#continue_invetigation
#show_statistics
#memorise_table

# dziala, ale nie u nas
# wyrzuca oczekiwany blad
# functions won't work if either user.name or user.email are empty in git config file
# blad komputera
hugo_share_object(model_lm)

hugo_get_object("pbiecek", "AdvancedR2018", "README.md")

hugo_share_secret(model_lm, "noga")

#rm(list = ls(), envir = .GlobalEnv) #usunbelismy wszystko
#a teraz chcemy odnowic
# hugo_continue #nie ma tego :<
#przypal




library(hugo)
hugo_start_investigation("h:/Windows7/Desktop/New folder/invest")

HR <- hugo_read_data("https://raw.githubusercontent.com/pbiecek/AdvancedR2018/master/Projekt2/HR_data.csv", file_extension = "csv", header = TRUE, separator = "", decimal = ".")

HR_clean <- hugo_clean_data(HR)

hugo_summarise_data(HR_clean)

HR$left <- as.factor(HR$left)

not_left <- sample(which(HR$left==0), 100)
left <- sample(which(HR$left==1), 100)

subs <- HR[c(left, not_left),]
hugo_train_model(subs, left~.)
#zwrocilo najlepszy model - randomForest

#tworzymy próbe testowa i treningowa:
not_left <- which(HR$left==0)
left <- which(HR$left==1)

n_l <- sample(not_left, round(2*length(not_left)/3))
l <- sample(left, round(2*length(left)/3))

tr <- HR[c(n_l, l),]
test <- HR[-c(n_l,l),]

model <- glm(left~., data = tr, family = "binomial")

hugo_memorise_model(model)

summary(model)
pred <- predict(model, newdata = test[,-7], type = "response" )
pr <- ifelse(pred>0.5, 1, 0)
tab <- table(pr, y = test$left)
BA <- 0.5*(tab[1,1]/sum(tab[,1])+tab[2,2]/sum(tab[,2]))

hugo_memorise(BA)

hugo_memorise_plot(plot(pred[test$left==1]), "osoby odeszły")
hugo_memorise_plot(plot(pred[test$left==0]), "osoby zostały")

hugo_save_investigation()

hugo_share_object(BA)#???

hugo_show_statistics()

hugo_show_history()

library(ranger)
library(randomForest)
library(caret)
library(breakDown)
library(dplyr)
library(ggplot2)
library(mlbench)


HR_data$left <- factor(HR_data$left)

HR_final_result <- list()
for(l in seq(50,300,50)) {
  print(l)
  num_prob = 10
  recalls_ranger_final <- rep(0, num_prob)
  time_ranger <- rep(0, num_prob)
  recalls_rf_final <- rep(0, num_prob)
  time_rf <- rep(0, num_prob)
  k <- 5
  for(j in 1:num_prob) {
    print(j)
    flds <- createFolds(HR_data$left, k = k, list = TRUE)
    system.time({
    recalls_ranger <- rep(0, 5)
    for(i in 1:k){
      train = HR_data[unlist(flds[-i]),]
      test = HR_data[unlist(flds[i]),]
      model_ranger <- ranger(left ~ ., data = train, importance = 'impurity', num.trees=l, probability=TRUE, classification = TRUE, mtry = 5)
      prediction <- predict(model_ranger, test)
      result <- table(as.integer(prediction$predictions[,2] > 0.5), test$left)
      recalls_ranger[i] <- result[2,2]/sum(result[,2])
    }
    }) -> time
    recalls_ranger_final[j] <- mean(recalls_ranger)
    time_ranger[j] <- time[3]
    
    recalls_rf <- rep(0, 5)
    system.time({
      recalls_ranger <- rep(0, 5)
      for(i in 1:k){
        train = HR_data[unlist(flds[-i]),]
        test = HR_data[unlist(flds[i]),]
        model_rf <- randomForest(left ~ ., data = train, importance = TRUE, ntree = l, mtry = 5)
        prediction <- predict(model_rf, test, type = "prob")
        result <- table(as.integer(prediction[,2] > 0.5), test$left)
        recalls_rf[i] <- result[2,2]/sum(result[,2])
      }
    }) -> time
    
    recalls_rf_final[j] <- mean(recalls_rf)
    time_rf[j] <- time[3]
  }
  res <- list(recalls_ranger_final,
     time_ranger,
     recalls_rf_final,
     time_rf)
  print(res)
  HR_final_result <- append(HR_final_result, list(l, res))
}


save(HR_final_result, file = "HR_final_result")

load("final_result")

final_result <- sonar_final_result

model_name <- c()
recall <- c()
time <- c()
ntree <- c()
for(i in seq(2,length(final_result),2)) {
  len <- length(final_result[[i]][[1]])
  ntree <- c(ntree, c(rep(final_result[[i-1]], len * 2)))
  model_name <- c(model_name, c(rep("ranger", len), rep("randomForest", len)))
  recall <- c(recall, c(final_result[[i]][[1]], final_result[[i]][[3]]))
  time <- c(time, c(final_result[[i]][[2]], final_result[[i]][[4]]))
}

result_df <- data.frame(model = model_name, ntree = ntree, recall, time)

ggplot(aes(y = recall, x = factor(ntree), fill = model), data = result_df) + 
  geom_boxplot() + 
  xlab("ntree") +
  ggtitle("Recall for Sonar dataset - randomForest vs ranger")



result_df %>%
  group_by(ntree, model) %>%
  summarise(time = mean(time, na.rm=TRUE)) -> time_df


ggplot(aes(y = time, x = ntree, color = model), data = time_df) + 
  geom_line(size=2) + 
  geom_point(size=3) + 
  ylab("time [s]") +
  ggtitle("Computation time for Sonar dataset - randomForest vs ranger")




## tree info
treeInfo(model_ranger)


## Sonar dataset
data(Sonar)

?ranger
?randomForest
sonar_final_result <- list()
for(l in seq(50,300,50)) {
  print(l)
  num_prob = 10
  recalls_ranger_final <- rep(0, num_prob)
  time_ranger <- rep(0, num_prob)
  recalls_rf_final <- rep(0, num_prob)
  time_rf <- rep(0, num_prob)
  k <- 5
  for(j in 1:num_prob) {
    print(j)
    flds <- createFolds(Sonar$Class, k = k, list = TRUE)
    system.time({
      recalls_ranger <- rep(0, 5)
      for(i in 1:k){
        train = Sonar[unlist(flds[-i]),]
        test = Sonar[unlist(flds[i]),]
        model_ranger <- ranger(Class ~ ., data = train, importance = 'impurity', num.trees=l, mtry=10, probability=TRUE, classification = TRUE)
        prediction <- predict(model_ranger, test)
        result <- table(as.integer(prediction$predictions[,2] > 0.5), test$Class)
        recalls_ranger[i] <- result[2,2]/sum(result[,2])
      }
    }) -> time
    recalls_ranger_final[j] <- mean(recalls_ranger)
    time_ranger[j] <- time[3]
    
    recalls_rf <- rep(0, 5)
    system.time({
      recalls_ranger <- rep(0, 5)
      for(i in 1:k){
        train = Sonar[unlist(flds[-i]),]
        test = Sonar[unlist(flds[i]),]
        model_rf <- randomForest(Class ~ ., data = train, importance = TRUE, ntree = l, mtry=10)
        prediction <- predict(model_rf, test, type = "prob")
        result <- table(as.integer(prediction[,2] > 0.5), test$Class)
        recalls_rf[i] <- result[2,2]/sum(result[,2])
      }
    }) -> time
    
    recalls_rf_final[j] <- mean(recalls_rf)
    time_rf[j] <- time[3]
  }
  res <- list(recalls_ranger_final,
              time_ranger,
              recalls_rf_final,
              time_rf)
  print(res)
  sonar_final_result <- append(sonar_final_result, list(l, res))
  print(sonar_final_result)
}

save(sonar_final_result, file="sonar_final_result")

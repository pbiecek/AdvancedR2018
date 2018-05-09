install.packages("ranger")
install.packages("caret")
install.packages("microbenchmark")
install.packages("survival")

library(ranger)
library(caret)
library(microbenchmark)
library(survival)

## Simple model for iris dataset
{
  folds <- createFolds(iris$Species, k = 5)
  iris_train <- iris[unlist(folds[-1]),]
  iris_test <- iris[folds[[1]],]
}

iris_ranger_model <- ranger(Species ~ ., data = iris_train, num.trees=30)
prediction <- predict(iris_ranger_model, iris_test)$prediction
prediction
?ranger

# Accuracy
sum(iris_test$Species == prediction)/length(prediction)

# Probability
iris_ranger_model <- ranger(Species ~ ., data = iris_train, num.trees=500, probability = TRUE)
predict(iris_ranger_model, iris_test)$prediction

# For very large datasets you may consider using "save.memory" option but it will 
# increase computation time
microbenchmark(
  ranger(Species ~ ., data = iris_train, num.trees=1000, mtry=4, num.threads=4, save.memory = TRUE)
)

# Use "write.forest" to reduce size of model
model <- ranger(Species ~ ., data = iris_train, num.trees=2000, write.forest = TRUE)
object.size(model)
# ~1519960 bytes #

model <- ranger(Species ~ ., data = iris_train, num.trees=2000, write.forest = FALSE)
object.size(model)
# 5464 bytes #

## Tree info - it will FAIL due to not saved trees
treeInfo(model, tree = 30)

model <- ranger(Species ~ ., data = iris_train, num.trees=2000, write.forest = TRUE)
treeInfo(model, tree = 30)

## Survival trees
data(veteran)

head(veteran)
# The variables in veteran are: 
# * trt: 1=standard 2=test * celltype: 1=squamous, 2=small cell, 3=adeno, 4=large * 
# * time: survival time in days * status: censoring status * karno: Karnofsky performance score (100=good) * 
# * diagtime: months from diagnosis to randomization * age: in years * prior: prior therapy 0=no, 10=yes
veteran_ranger_model <- ranger(Surv(time, status) ~ .,
                data = veteran,
                mtry = 4,
                importance = "impurity",
                splitrule = "extratrees",
                verbose = TRUE,
                write.forest =TRUE)

# Variable importance plot
variable <- names(veteran_ranger_model$variable.importance)
importance <- veteran_ranger_model$variable.importance
ggplot(data.frame(variable, importance), aes(x=reorder(variable, importance), y=importance)) + 
  geom_bar(stat="identity", position="dodge") + 
  xlab("variable") + 
  coord_flip() 

# Model death time
{
  # Average
  death_times <- veteran_ranger_model$unique.death.times 
  surv_prob <- data.frame(veteran_ranger_model$survival)
  avg_prob <- sapply(surv_prob, mean)
  
  # Cell type 
  smallcell_cell <- veteran$celltype == "smallcell"
  smallcell_death_times <- veteran_ranger_model$unique.death.times
  smallcell_surv_prob <- data.frame(veteran_ranger_model$survival[smallcell_cell,])
  smallcell_avg_prob <- sapply(smallcell_surv_prob, mean)
  
  squamous_cell <- veteran$celltype == "squamous"
  squamous_death_times <- veteran_ranger_model$unique.death.times
  squamous_surv_prob <- data.frame(veteran_ranger_model$survival[squamous_cell,])
  squamous_avg_prob <- sapply(squamous_surv_prob, mean)
}

# Plot for cell types
{
  plot(death_times, 
       avg_prob, 
       lwd = 3,  
       type = "l", 
       ylim = c(0,1),
       col = "black",
       xlab = "Days",
       ylab = "survival",
       main = "Survival Curves by cell type")
  lines(smallcell_death_times, smallcell_avg_prob, lwd = 3, col="red")
  lines(squamous_death_times, squamous_avg_prob, lwd = 3, col="green")
  legend(600, 0.9, legend = c('Average', "Small cell", "Squamous"), fill=c("black", "red", "green"))
}

max_karno_id <- which.max(veteran$karno)
min_karno_id <- which.min(veteran$karno)

veteran[max_karno_id,]
# Plot the survival models for some patients
{
  plot(veteran_ranger_model$unique.death.times,
       veteran_ranger_model$survival[max_karno_id,], 
       type = "l", 
       ylim = c(0,1),
       col = "green",
       xlab = "Days",
       ylab = "survival",
       main = "Patient Survival Curves")
  
  lines(veteran_ranger_model$unique.death.times, 
        veteran_ranger_model$survival[min_karno_id,], 
        type = "l", col = "red")
}



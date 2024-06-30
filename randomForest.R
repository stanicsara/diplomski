#install.packages("randomForest")
library(randomForest)
library(caret)
source('util.R')

test_rf <- readRDS("test.RDS")
train_rf <- readRDS("train.RDS")

#kreiranje prvog modela
set.seed(1)
rf1 <- randomForest(high_revenue ~ ., data = train_rf)
print(rf1) #OOB error 11.88% 
           #ntree 500
           #mtry 3

rf1_pred <- predict(rf1, newdata = test_rf, type = "class")
cm1_rf <- table(actual = test_rf$high_revenue,
             predicted = rf1_pred)
cm1_rf

eval1_rf <- compute_eval_metrics(cm1_rf)
eval1_rf

varImpPlot(rf1)

#trazenje optimalne vrednosti za mtry parametar

control <- trainControl(method='cv', 
                        number=10)
set.seed(1)
mtry <- sqrt(ncol(train_rf[,-15]))
mtry_grid <- expand.grid(.mtry=mtry)
mtry_train <- train(x = train_rf[,-15],
                    y = train_rf$high_revenue,
                    method='rf', 
                    metric='Accuracy', 
                    tuneGrid=mtry_grid, 
                    trControl=control)
print(mtry_train) # mtry 3.872983
best_mtry <- mtry_train$bestTune$mtry

#drugi model

set.seed(1)
rf2 <- randomForest(high_revenue ~ ., data = train_rf, mtry = best_mtry)
print(rf2) #OOB error 11.75% 
           #ntree 500
           #mtry 4

rf2_pred <- predict(rf2, newdata = test_rf, type = "class")
cm2_rf <- table(actual = test_rf$high_revenue,
                predicted = rf2_pred)
cm2_rf

eval2_rf <- compute_eval_metrics(cm2_rf)
eval2_rf


data.frame(rbind(eval1_rf, eval2_rf), row.names = paste("RF", 1:2))

# accuracy precision    recall        F1
# RF 1 0.8922261 0.8283379 0.7169811 0.7686473
# RF 2 0.8934040 0.8257373 0.7264151 0.7728984

#Drugi model je dao bolje rezultate
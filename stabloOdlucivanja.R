
### PRVI ALGORITAM - DECISION TREE 


library(rpart)
#install.packages('rpart.plot')
library(rpart.plot)
source('util.R')

movies <- readRDS("movies_final.RDS")

#kreiramo test i train set

set.seed(1)
train_indices <- createDataPartition(movies$high_revenue, p = 0.8, list = FALSE)
train_data <- movies[train_indices, ]
test_data <- movies[-train_indices, ]

prop.table(table(train_data$high_revenue))
prop.table(table(test_data$high_revenue))

#cuvamo datasetove da bi ih koristili za naredne modele

saveRDS(train_data, "train.RDS")
saveRDS(test_data, "test.RDS")


#prvi model

set.seed(1)
tree1 <- rpart(high_revenue ~ ., data = train_data)

rpart.plot(tree1, extra = 104)

tree1_pred <- predict(tree1, newdata = test_data, type = "class")
cm1_dt <- table(actual = test_data$high_revenue,
             predicted = tree1_pred)
cm1_dt

eval1_dt <- compute_eval_metrics(cm1_dt)
eval1_dt

#drugi model

set.seed(1)
tree2 <- rpart(high_revenue ~ ., data = train_data, 
               control = rpart.control(minsplit = 10, cp = 0.001))

rpart.plot(tree2, extra = 104)

tree2_pred <- predict(tree2, newdata = test_data, type = "class")
cm2_dt <- table(actual = test_data$high_revenue,
             predicted = tree2_pred)
cm2_dt

eval2_dt <- compute_eval_metrics(cm2_dt)
eval2_dt


#predikcije na train setu zbog provere overfitovanja

tree2_pred_train <- predict(tree2, newdata = train_data, type = "class")
cm2_train_dt <- table(actual = train_data$high_revenue,
             predicted = tree2_pred_train)
cm2_train_dt

eval2_train_dt <- compute_eval_metrics(cm2_train_dt)
eval2_train_dt

data.frame(rbind(eval2_dt, eval2_train_dt), row.names = paste("Eval ", 1:2)) #dobili smo mnogo bolje vrednosti metrika na train setu sto znaci
                                                                       #da je ovaj model overfitovan


#trazimo optimalnu vrednost za cp kroz krosvalidaciju

library(e1071)

tr_ctrl <- trainControl(method = "cv", number = 10)
cp_grid <- expand.grid( .cp = seq(0.001, 0.02, 0.0005))

set.seed(1)
tree_cv <- train(x = train_data[,-16],
                 y = train_data$high_revenue,
                 method = 'rpart', 
                 trControl = tr_ctrl,
                 tuneGrid = cp_grid, 
                 minsplit = 10)

tree_cv
plot(tree_cv)
best_cp <- tree_cv$bestTune$cp #dobijamo 0.002 kao najbolju vrednost za cp

#treci model

set.seed(1)
tree3 <- rpart(high_revenue ~ ., data = train_data, 
               control = rpart.control(minsplit = 10, cp = best_cp))

rpart.plot(tree3, extra = 104)
#names(tree3)
#tree3$variable.importance

#pravljenje predikcija

tree3_pred <- predict(tree3, newdata = test_data, type = "class")
cm3_dt <- table(actual = test_data$high_revenue,
             predicted = tree3_pred)
cm3_dt

eval3_dt <- compute_eval_measures(cm3_dt)
eval3_dt

#train data predikcije

tree3_pred_train <- predict(tree3, newdata = train_data, type = "class")
cm3_train_dt <- table(actual = train_data$high_revenue,
                   predicted = tree3_pred_train)
cm3_train_dt

eval3_train_dt <- compute_eval_metrics(cm3_train_dt)
eval3_train_dt

data.frame(rbind(eval3_dt, eval3_train_dt), row.names = paste("Eval ", 1:2)) # dobili smo jos jedan overfitovan model

#manji raspon za cp da bi se dobilo citljivije stablo

cp_grid_2 <- expand.grid( .cp = seq(0.005, 0.02, 0.0005))

set.seed(1)
tree_cv_2 <- train(x = train_data[,-16],
                 y = train_data$high_revenue,
                 method = 'rpart', 
                 trControl = tr_ctrl,
                 tuneGrid = cp_grid_2, 
                 minsplit = 20)

tree_cv_2
plot(tree_cv_2)
best_cp_2 <- tree_cv_2$bestTune$cp 

#cetvrti model

set.seed(1)
tree4 <- rpart(high_revenue ~ ., data = train_data, 
               control = rpart.control(minsplit = 20, cp = best_cp_2))

rpart.plot(tree4, extra = 104)

tree4_pred <- predict(tree4, newdata = test_data, type = "class")
cm4_dt <- table(actual = test_data$high_revenue,
             predicted = tree4_pred)
cm4_dt

eval4_dt <- compute_eval_metrics(cm4_dt)
eval4_dt

data.frame(rbind(eval1_dt, eval4_dt), row.names = paste("Tree_", 1:2))

#Tree4 model je dao bolje rezultate

# accuracy precision    recall        F1
# Tree_ 1 0.8527680 0.8107143 0.5353774 0.6448864
# Tree_ 2 0.8651355 0.7600000 0.6721698 0.7133917




############## DRUGI MODEL NAIVNI BAJES #################

test_nb <- readRDS("test.RDS")
train_nb <- readRDS("train.RDS")
source('util.R')

# provera normalnosti

num_vars <- c(1:8)

apply(train_nb[,num_vars], 2, ad.test)
apply(test_nb[,num_vars], 2, ad.test)
# nemaju normalnu raspodelu

#diskretizacija train seta

library(bnlearn)

to_discretize <- c(1:8)

transformed_data_train <- discretize(train_nb[,to_discretize],
                               method = "quantile",
                               breaks = 5)
summary(transformed_data_train) #dobijamo neujednacene vrenosti za release_year, point, runtime, release_month, pa cemo promeniti broj intervala

ggplot(train_nb,
       aes(x = release_year)) +
  geom_histogram() + 
  theme_minimal()

ggplot(train_nb,
       aes(x = point)) +
  geom_histogram() + 
  theme_minimal()

ggplot(train_nb,
       aes(x = runtime)) +
  geom_histogram() + 
  theme_minimal()

ggplot(train_nb,
       aes(x = release_month)) +
  geom_histogram() + 
  theme_minimal()

transformed_data_train <- discretize(train_nb[,to_discretize],
                               method = "quantile",
                               breaks = c(3,3,5,5,5,3,5,5))
summary(transformed_data_train) #release_month cemo da izbacimo iz daljeg razmatranja jer ne mozemo da podelimo na intervale, a da dobijemo priblizno ujednacen broj

# diskretizacija test seta
transformed_data_test <- discretize(test_nb[,to_discretize],
                               method = "quantile",
                               breaks = 5)
summary(transformed_data_test) 

ggplot(test_nb,
       aes(x = release_year)) +
  geom_histogram() + 
  theme_minimal()

ggplot(test_nb,
       aes(x = point)) +
  geom_histogram() + 
  theme_minimal()

ggplot(test_nb,
       aes(x = runtime)) +
  geom_histogram() + 
  theme_minimal()

ggplot(test_nb,
       aes(x = release_month)) +
  geom_histogram() + 
  theme_minimal()

transformed_data_test <- discretize(test_nb[,to_discretize],
                               method = "quantile",
                               breaks = c(3,2,5,5,5,5,5,5))
summary(transformed_data_test) #izbacujemo release_month i ovde
train_nb$release_month <-NULL
test_nb$release_month <-NULL

#pravimo novi train dataset sa diskretizovanim kolonama

to_add_col <- setdiff(colnames(train_nb), colnames(transformed_data_train))
to_add_col
train_nb <- cbind(transformed_data_train[,-8],
                          train_nb[,to_add_col])

#pravimo novi test dataset sa diskretizovanim kolonama

to_add_col_test <- setdiff(colnames(test_nb), colnames(transformed_data_test))
test_nb <- cbind(transformed_data_test[,-8],
                    test_nb[,to_add_col_test])



#prvi model
library(e1071)

set.seed(1)
nb1 <- naiveBayes(high_revenue ~ ., data = train_nb)
print(nb1)

nb1_pred <- predict(nb1, newdata = test_nb, type = "class")
cm1_nb <- table(actual = test_nb$high_revenue,
             predicted = nb1_pred)
cm1_nb

eval1_nb <- compute_eval_metrics(cm1_nb)
eval1_nb

########################## ROC krive

nb1_pred_prob <- predict(nb1, test_df, type = 'raw')
head(nb1_pred_prob)

# Balansiranje podataka 

library(caret)
library(DMwR2)
#install.packages('ROSE')
library(ROSE)
#installed.packages("naivebayes")
library(naivebayes)
library(themis)

ctrl <- trainControl(method = "repeatedcv", repeats = 5,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary,
                     sampling = "down")
#down-sampling

set.seed(1)
down_train <- train(x = train_nb[,-15],
                    y = train_nb$high_revenue,
                    method = "naive_bayes",
                    metric = "ROC",
                    trControl = ctrl)

#up-sampling

ctrl$sampling <- "up"
set.seed(1)
up_train <- train(x = train_nb[,-15],
                    y = train_nb$high_revenue,
                    method = "naive_bayes",
                    metric = "ROC",
                    trControl = ctrl)

#ROSE

ctrl$sampling <- "rose"
set.seed(1)
rose_train <- train(x = train_nb[,-15],
                    y = train_nb$high_revenue,
                    method = "naive_bayes",
                    metric = "ROC",
                    trControl = ctrl)

#originalni skup podataka

ctrl$sampling <- NULL
set.seed(1)
original_train <- train(x = train_nb[,-15],
                    y = train_nb$high_revenue,
                    method = "naive_bayes",
                    metric = "ROC",
                    trControl = ctrl)


inside_models <- list(original = original_train,
                      down = down_train,
                      up = up_train,
                      ROSE = rose_train)
inside_resampling <- resamples(inside_models)
summary(inside_resampling, metric = "ROC")

# ROC 
#             Min.   1st Qu.    Median      Mean   3rd Qu.      Max.    NA's
# original 0.8678893 0.8859544 0.8914533 0.8928718 0.9014265 0.9194867    0
# down     0.8665629 0.8865657 0.8914965 0.8927525 0.9014864 0.9192215    0
# up       0.8681892 0.8863437 0.8914937 0.8928951 0.9006515 0.9198558    0
# ROSE     0.8683622 0.8841061 0.8912918 0.8927241 0.9001224 0.9177797    0

#rezultati za svaki model su skoro identicni, ali koristicemo up model za balansiranje jer u odnosu na down model necemo gubiti podatke

set.seed(1)
train_up <- upSample(x = train_nb[,-15], y = train_nb$high_revenue)
colnames(train_up)[15] <- "high_revenue"
str(train_up)
table(train_up$high_revenue)

#kreiranje drugog modela

nb2 <- naiveBayes(high_revenue ~ ., data = train_up)
print(nb2)

nb2_pred <- predict(nb2, newdata = test_nb, type = "class")
cm2_nb <- table(actual = test_nb$high_revenue,
                predicted = nb2_pred)
cm2_nb

eval2_nb <- compute_eval_metrics(cm2_nb)
eval2_nb

#raw predikcije za drugi model

#install.packages('pROC')
library(pROC)

nb2_pred_prob <- predict(nb2, test_nb, type = 'raw')
head(nb2_pred_prob)

nb2_roc <- roc(response = as.integer(test_nb$high_revenue),
               predictor = nb2_pred_prob[,2],
               levels = c(1,2))
nb2_roc$auc #0.76

plot.roc(nb2_roc,
         print.thres = 'best',
         print.thres.best.method = "youden") #0.418 (0.600 0.788)

nb2_coords <- coords(nb2_roc,
                     x = "local maximas",
                     ret = c("threshold", "sensitivity", "specificity", "accuracy", "precision"),
                     transpose = FALSE)
nb2_coords

nb2_pred2 <- ifelse(test = nb2_pred_prob[,2] > 0.418,
                    yes = "Yes",
                    no = "No")
nb2_pred2 <- as.factor(nb2_pred2)
cm3_nb <- table(actual = test_nb$high_revenue,
                predicted = nb2_pred2)
cm3_nb
eval3_nb <- compute_eval_metrics(cm3_nb)
eval3_nb

data.frame(rbind(eval1_nb, eval2_nb, eval3_nb), row.names = paste("NB", 1:3))

# accuracy precision    recall        F1
# NB 1 0.7685512 0.5589354 0.3466981 0.4279476
# NB 2 0.7020024 0.4309764 0.6037736 0.5029470
# NB 3 0.6466431 0.3957346 0.7877358 0.5268139


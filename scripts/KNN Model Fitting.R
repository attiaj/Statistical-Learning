library(MASS)
library(dplyr)
library(tidyverse)
data(Boston)

#Fetch Boston data, "medv" is response variable, "lstat" is predictor
head(Boston[, c("medv", "lstat")])

#kknn package has knn regression capability, not just regression
library(kknn)

knn_fit <- kknn(medv ~ lstat,
                train = Boston,
                test = Boston, 
                k = 30) #fit the model

#add fit values to dataset
knn_estimates <- mutate(Boston, knn_est = fitted(knn_fit)) |>
  arrange(knn_est)
knn_estimates |>
  select(medv, lstat, knn_est)

#plot fitted line on scatter plot
knn_estimates |>
  ggplot(aes(x = lstat, y = medv)) +
  geom_point(size = 0.5) +
  geom_line(aes(y = knn_est), color = "blue", linewidth = 2)

#Choosing k: k = 1 -> overfitting, k = 300 -> oversmoothing, ideal k is somewhere between

#MSE for k = 30 training set -> ~25.2
mean((knn_estimates$medv - knn_estimates$knn_est)^2)

#k = 1 training set results in less MSE, but overfitting
knn_fit_k1 <- kknn(medv ~ lstat,
                train = Boston,
                test = Boston, 
                k = 1) #fit the model

knn_estimates_k1 <- mutate(Boston, knn_est = fitted(knn_fit_k1)) |>
  arrange(knn_est)
mean((knn_estimates_k1$medv - knn_estimates_k1$knn_est)^2) 

#data splitting
set.seed(1234567)

n <- nrow(Boston)
index <- sample(x = 1:n,
                size = round(0.8*n),
                replace = FALSE)

train <- Boston[index, ]
test <- Boston[-index, ]

#calculate train/test MSE 
knn_train_fit <- kknn(medv ~lstat,
                      train = train,
                      test = train, k = 30)
train_MSE <- mean((train$medv - knn_train_fit$fitted.values)^2)
train_MSE

knn_train_fit <- kknn(medv ~lstat,
                      train = train,
                      test = test, k = 30)
test_MSE <- mean((test$medv - knn_train_fit$fitted.values)^2)
test_MSE

#Find the optimal k that minimizes MSE on the test set
kgrid <- 1:100

#lapply to apply function to list/vector
test_preds <- lapply(X = kgrid,
                     FUN = function(x){
                       kknn(medv ~ lstat,
                            train = train,
                            test = test,
                            k = x)
                     })

test_MSEs <- lapply(test_preds, FUN = function(x){
  mean((test$medv - x$fitted.values)^2)
})

#optimal k = 30
k_opt <- kgrid[which.min(test_MSEs)]
k_opt

#Fit to entire data set using optimal k
knn_best_fit <- kknn(medv ~ lstat,
                     train = Boston,
                     test = Boston,
                     k = k_opt) 

#Now, find optimal k using 5-fold cross validation:
library(caret)
set.seed(1001)

#create dataframe, necessary for following method
kgrid <- expand.grid(k = c(1:100))

cv <- trainControl(method = "cv",
                   number = 5)

knn_fit <- train(medv ~ lstat,
                 data = Boston,
                 method = "knn",
                 tuneGrid = kgrid,
                 trControl = cv)

plot(knn_fit)
#optimal k = 36

#fit on full dataset with optimal k 
k_opt <- knn_fit$bestTune$k
knn_tuned <- train(medv ~ lstat,
                   data = Boston,
                   method = "knn",
                   tuneGrid = expand.grid(k = k_opt),
                   trControl = trainControl(method = "none"))

#Now use LOOCV instead (more computation time) 
kgrid <- expand.grid(k = 1:100)
loo <- trainControl(method = "LOOCV")

fit <- train(medv ~ lstat,
             data = Boston,
             method = "knn",
             tuneGrid = kgrid,
             trControl = loo)
plot(fit)

#Now use bootstrapping
library(rsample)
set.seed(10)

boot_sample <- bootstraps(Boston, times = 5)
boot_sample

boot_1 <- training(boot_sample$splits[[1]])
dim(boot_1)

oob_1 <- testing(boot_sample$splits[[1]])
dim(oob_1)

#do 500 bootstraps, view histogram showing the percent of observations left out 
boot_sample <- bootstraps(Boston, times = 500)
oob_percent <- sapply(boot_sample$splits,
                      function(s){nrow(testing(s))/nrow(training(s))})
ggplot() +
  geom_histogram(aes(x = oob_percent), bins = 20) +
  theme_bw(base_size = 18)

#tune k using bootstrapping

set.seed(1001)
kgrid <- expand.grid(k = 1:100)
boot <- trainControl(method = "boot",
                     number = 25)

#model fit
boot_tuned_knn <- train(medv ~lstat,
                        data = Boston,
                        method = "knn",
                        trControl = boot,
                        tuneGrid = kgrid)
plot(boot_tuned_knn)

#Now compare simple linear regression to KNN to determine which is more accurate

set.seed(51)
index <- createDataPartition(Boston$medv,
                             p = 0.8,
                             list = FALSE,
                             times = 1)

train <- Boston[index, ]
test <- Boston[-index, ]

#tune KNN parameter using 10 fold CV
kgrid <- expand.grid(k = 1:100)
cv <- trainControl(method = 'cv',
                   number = 10)

knn_fit <- train(medv ~ lstat,
                 data = train,
                 method = "knn",
                 tuneGrid = kgrid,
                 trControl = cv)

k_opt <- knn_fit$bestTune$k

knn_fit$results[k_opt, ]

#fit knn using optimal k value
knn_tuned <- train(medv ~ lstat,
                   data = train,
                   method = "knn",
                   tuneGrid = knn_fit$bestTune,
                   trControl = trainControl(method = "none"))

#fit SLR model

slr_fit <- train(medv ~ lstat,
                 data = train,
                 method = "lm",
                 trControl = trainControl(method = "none"))

summary(slr_fit)

#compare SLR and KNN

knn_preds <- predict(knn_tuned, newdata = test)
slr_preds <- predict(slr_fit, newdata = test)

mean((test$medv-knn_preds)^2)
mean((test$medv-slr_preds)^2)

#KNN MSE = 33.4, SLR MSE = 46.7, KNN is the more accurate predictor for this predictor/response set!
library(MASS)
library(klaR)
library(tidyverse)
library(caret)
library(rsample)
library(ISLR2)
library(knitr)
library(AppliedPredictiveModeling)
library(kableExtra)
library(nnet)
library(glmnet)

# Read the data
wines <- read.csv("wine.data", header = FALSE)
# Add the column names
colnames(wines) <- c("Class", "Alcohol", "Malic", "Ash", 
                     "Alcal", "Mg", "Phenol",
                     "Flav", "Nonf", "Proan",
                     "Color", "Hue", "Abs", "Proline")
#convert our response to a factor
wines$Class <- as.factor(wines$Class)

#frequency of each class of wine
table(wines$Class)

## 30-NN Classifier / with no tuning done
fit <- train(Class ~ Alcohol + Malic,
             data = wines,
             method = "knn",
             tuneGrid = data.frame(k = 30),
             trControl = trainControl(method = "none"))
fit

#Plot decision boundary
grid <- expand.grid(
  Alcohol = seq(min(wines$Alcohol), max(wines$Alcohol), length.out = 200),
  Malic = seq(min(wines$Malic), max(wines$Malic), length.out = 200)
)
grid$Class <- predict(fit, newdata = grid)

# Plot
ggplot() +
  geom_tile(data = grid, aes(x = Alcohol, y = Malic, fill = Class), alpha = 0.3) +
  geom_point(data = wines, aes(x = Alcohol, y = Malic, color = Class))

head(wines, n = 4)

#Tune K using 5-fold CV
set.seed(1001)
## K values for tuning
kgrid <- expand.grid(k = seq(1,51, by=2))
## 5-fold CV, repeated, tuning
tr <- trainControl(method = "repeatedcv",
                   number = 5,
                   repeats = 50)
## Train the classifier
fit <- train(Class ~ Alcohol + Malic,
             data = wines,
             method = "knn",
             tuneGrid = kgrid,
             trControl = tr)
plot(fit)

fit$bestTune$k #optimal k = 21

## Refit the model with best K
tuned_knn_class <- train(Class ~ Alcohol + Malic,
                         data = wines,
                         method = "knn",
                         tuneGrid = expand.grid(k = fit$bestTune$k),
                         trControl = trainControl(method = "none"))

#predict new observations using the tuned fit
new_data = data.frame(Alcohol = c(12.78, 13), Malic = c(2, 3))

pred_class <- predict(tuned_knn_class,
                      newdata = new_data)
pred_class

#Use prob parameter to print conditional probabilities of new
#observations being in each class
pred_prob <- predict(tuned_knn_class,
                     newdata = new_data,
                     type = "prob")
pred_prob


#split the original data
index <- createDataPartition(wines$Class, p = 0.7, list = FALSE)
#get the train and test sets
train <- wines[index,]
test <- wines[-index,]

## K values for tuning
kgrid <- expand.grid(k = seq(1,51, by=2))
## 5-fold CV tuning
tr <- trainControl(method = "cv",
                   number = 5)
## Train the classifier
fit <- train(Class ~ Alcohol + Malic,
             data = train,
             method = "knn",
             tuneGrid = kgrid,
             trControl = tr)
fit$bestTune$k

## Refit the model with best K
tuned_knn_class <- train(Class ~ Alcohol + Malic,
                         data = train,
                         method = "knn",
                         tuneGrid = expand.grid(k = fit$bestTune$k),
                         trControl = trainControl(method = "none"))
#predict on the test set
preds <- predict(tuned_knn_class, test)

#confusion matrix
confusionMatrix(test$Class, preds)
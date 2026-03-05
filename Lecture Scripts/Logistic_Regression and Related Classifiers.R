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

heart_data <- read_csv("https://www4.stat.ncsu.edu/online/datasets/heart.csv") |>
  filter(RestingBP > 0) |> #remove one value
  mutate(HeartDiseaseFac = factor(HeartDisease))

#Fit logistic regression model with age, sex predicting Heart Disease
#"glm" = generalized linear models
log_reg_fit <- glm(HeartDiseaseFac ~ Age + Sex, 
                   data = heart_data, family = "binomial")
summary(log_reg_fit)

#"SexM" in summary represents indicator for males

#Add an interaction term for Age*Sex
log_reg_fit_int <- glm(HeartDiseaseFac ~ Age + Sex + Age*Sex, 
                       data = heart_data, family = "binomial")
summary(log_reg_fit_int)

# Logistic regression adding in RestingBP
heart_glm =  glm(HeartDiseaseFac ~ Age + RestingBP + Sex, 
                 family = "binomial", 
                 data = heart_data)

heart_coef <- heart_glm$coefficients
heart_coef

#Predict chance of heart disease for M, F age 60, BP 130
newx <- data.frame(Age = c(60, 60),
                   RestingBP = c(130, 130),
                   Sex = c("M", "F"))
predict(heart_glm, 
        newdata = newx,
        type = "response")

# Confusion matrix
confusionMatrix(table(predicted = ifelse(predict(heart_glm, type = "response") > 0.5,
                                         1, 0),
                      heart_data$HeartDiseaseFac))

#Confusion matrix for females only
confusionMatrix(table(predicted = ifelse(predict(heart_glm, 
                                                 newdata = heart_data |> filter(Sex == "F"), 
                                                 type = "response") > 0.5,
                                         1, 0),
                      heart_data |> 
                        filter(Sex == "F") |> 
                        pull(HeartDiseaseFac)
))

#Confusion matrix for males only
confusionMatrix(table(predicted = ifelse(predict(heart_glm, 
                                                 newdata = heart_data |> filter(Sex == "M"), 
                                                 type = "response") > 0.5,
                                         1, 0),
                      heart_data |> 
                        filter(Sex == "M") |> 
                        pull(HeartDiseaseFac)
))

summary(heart_glm)

set.seed(1102)
# CV to choose lambda for lasso
logit_cv <- cv.glmnet(x = as.matrix(heart_data |> 
                                      dplyr::select(-starts_with("Heart"), -Sex, -ChestPainType, -RestingECG, -ExerciseAngina, -ST_Slope)), 
                      y = heart_data$HeartDiseaseFac,
                      family = binomial(),
                      alpha = 1)

# Final fit with lambda chosen by 1-SE rule
heart_lasso <- glmnet(x = as.matrix(heart_data |> 
                                      dplyr::select(-starts_with("Heart"), -Sex, -ChestPainType, -RestingECG, -ExerciseAngina, -ST_Slope)),
                      y = heart_data$HeartDiseaseFac,
                      family = binomial(),
                      alpha = 1,
                      lambda = logit_cv$lambda.1se)
# Estimated coefs
coef(heart_lasso)

# Read the data
wines <- read_table("https://www4.stat.ncsu.edu/online/datasets/Wines.txt")
# classes of wine
table(wines$Class)

#Probs of each class of wine occurring
p_Y <- table(wines$Class)/nrow(wines)
p_Y

# new data set considering only alcohol
wine_small <- wines |>
  dplyr::select(Class, Alcohol) |>
  mutate(Class = as.factor(Class))

#calculate sample means, variances
means_variances <- wine_small |>
  group_by(Class) |>
  summarize(means = mean(Alcohol), vars = var(Alcohol))
means_variances

# new data
x <- 12
# density evaluated at x
f <- dnorm(x, 
           mean = means_variances$means, 
           sd = sqrt(means_variances$vars))
# p_k * f_k
pf <- p_Y * f
round(pf, 4) 
#We predict a wine with alcohol at 12 to be most likely class 2

#Calculate the actual probability of each class via Bayes Theorem
post_prob <- pf / sum(pf)
round(post_prob,3)

#Fit QDA model on Class ~ Alcohol
qda <- train(Class ~ Alcohol,
             data = wine_small,
             method = "qda")
Alcohol <- seq(10, 16, length.out = 501)
preds <- predict(qda, newdata = data.frame(Alcohol))

#Fit QDA model with multiple predictors
X <- cbind(wines$Alcohol, wines$Proline)
mu <- vector("list")
Sigma <- vector("list")
for(ii in 1:3){
  mu[[ii]] <- colMeans(X[wines$Class == ii, ])
  Sigma[[ii]] <- cov(X[wines$Class == ii, ])
}
mu

# For multivariate normal density
library(mnormt)
# new data
newx <- data.frame(Alcohol = 13, Proline = 600)
# p-hat
p <- table(wines$Class)/nrow(wines)
# f-hat
f <- c()
for(ii in 1:3){
  f[ii] <- dmnorm(newx, mean = mu[[ii]], varcov = Sigma[[ii]])
}
# Posterior prob
post_prob <- p*f / sum(p*f)
round(post_prob, 3)

#Can use qda() function to build more efficiently
wine_qda <- qda(Class ~ Alcohol + Proline, data = wines)
wine_qda

# prediction of new x
pred <- predict(wine_qda, newdata = newx)
pred

#Run the above model with CV
set.seed(1001)
caret_qda <- train(factor(Class) ~ Alcohol + Proline,
                   data = wines,
                   method = "qda",
                   trControl = trainControl(method = "CV",
                                            number = 10))
caret_qda$results

confusionMatrix(caret_qda)


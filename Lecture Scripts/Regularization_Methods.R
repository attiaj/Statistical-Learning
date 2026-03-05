
library(glmnet)
## model matrix (standardized) and response
medv <- Boston$medv
model_mat <- Boston[ , -13]|> 
  scale() |> #scale predictors
  as.matrix()

## Fit ridge regression for a grid of lambda
grid <- 10^seq(-2, 10, length = 100)
boston_ridge <- glmnet(y = medv, 
                       x = model_mat,
                       alpha = 0,
                       lambda = grid)
betahat <- coef(boston_ridge)
dim(betahat)

#Ridge regression using CV
set.seed(1001)
grid <- 10^seq(-2, 10, length = 100)
cv_out <- cv.glmnet(x = model_mat, y = medv, 
                    alpha = 0, 
                    lambda = grid)

# Plot cv results
plot(cv_out)

## lambda with minimum CV error/1 - SE
bestlam <- data.frame(min = cv_out$lambda.min,
                      one_se = cv_out$lambda.1se)
bestlam 

## Refit ridge regression
# The cv_out object already has the full data fit
# for each lambda
ridge_min = predict(cv_out$glmnet.fit, 
                    type = "coefficients", 
                    s = bestlam$min)
ridge_1se = predict(cv_out$glmnet.fit, 
                    type = "coefficients", 
                    s = bestlam$one_se)
# Least squares
ols <- coef(lm(medv ~ model_mat))
betahat <- cbind(ridge_min, ridge_1se, ols)
colnames(betahat) <- c("min", "1se", "ols")
rownames <- attributes(betahat)$Dimnames[[1]]
betahat

library(glmnet)
## model matrix (standardized) and response
medv <- Boston$medv
model_mat <- Boston[ , -13]|> 
  scale() |> 
  as.matrix()

## Fit lasso regression for a grid of lambda
grid <- 10^seq(-3, 7, length = 100)
boston_lasso <- glmnet(x = model_mat, y = medv,
                       alpha = 1,
                       lambda = grid)
beta_hat <- coef(boston_lasso)
dim(beta_hat)

##  Lasso cross-validation
set.seed(1001)
grid <- 10^seq(-3, 7, length = 100)
cv_out <- cv.glmnet(x = model_mat, y = medv, 
                    alpha = 1,
                    lambda = grid)
# Plot cv results
plot(cv_out)

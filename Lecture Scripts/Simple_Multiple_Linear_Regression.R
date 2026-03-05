library(tidyverse)

#Create dataset, change column names
bike_share <- read_csv("https://www4.stat.ncsu.edu/online/datasets/SeoulBikeData.csv",
                       local = locale(encoding = "latin1"))

bike_share |>
  select("Rented Bike Count", everything())

bike_share <- bike_share |>
  rename("date" = "Date",
         "rented_bike_count" = `Rented Bike Count`,
         "hour" = "Hour",
         "temperature" = `Temperature(°C)`,
         "humidity" = `Humidity(%)`,
         "wind_speed" = `Wind speed (m/s)`,
         "visibility" = `Visibility (10m)`,
         "dew_point_temperature" = `Dew point temperature(°C)`,
         "solar_radiation" = `Solar Radiation (MJ/m2)`,
         "rainfall" = `Rainfall(mm)`,
         "snowfall" = `Snowfall (cm)`,
         "seasons" = "Seasons",
         "holiday" = "Holiday",
         "functioning_day" = "Functioning Day" 
        ) |>
  mutate(date = dmy(date), #convert the date variable from character
         seasons = factor(seasons),
         holiday = factor(holiday),
         functioning_day = factor(functioning_day))

bike_share |>
  ggplot(aes(x = temperature, y = rented_bike_count)) +
  geom_point(size = 0.5) +
  geom_smooth(method = "lm")

#Fit using SLR with temperature as predictor, rented bike count as response
SLR_fit <- lm(rented_bike_count ~ temperature, data = bike_share)
summary(SLR_fit)$coefficients

#Create example matrix
bike_share_first_ten <- bike_share[1:10, ]
y <- bike_share_first_ten$rented_bike_count
X <- bike_share_first_ten |>
  mutate(intercept = rep(1, 10)) |>
  select(intercept, temperature, hour, wind_speed) |>
  as.matrix()
y
X

#%*% is the matrix muliplication operator in R
#solve() gives the matrix inverse, t() gives the transpose
#Implement unique sum-of-squares minimizer Bhat = (X^T*X)^-1*X^T*Y
solve(t(X)%*%X)%*%t(X)%*%y

#Above solution gives same coefficients as basic lm fit:
MLR_first_ten <- lm(rented_bike_count ~ temperature + hour + wind_speed,
                    data = bike_share_first_ten)
MLR_first_ten$coefficients

library(robustbase)
#Fit using absolute error loss
mlr_ae_fit <- lmrob.lar(x = as.matrix(bike_share |> 
                                        mutate(intercept = rep(1, nrow(bike_share))) |>
                                        select(intercept, temperature, wind_speed)),
                                        y = bike_share$rented_bike_count)
mlr_ae_fit$coefficients

#Fit using normal (LS) error
mlr_ls_fit <- lm(rented_bike_count ~ temperature + wind_speed, data = bike_share)
mlr_ls_fit$coefficients

#Robust regression methods useful for data with outliers or heavy tails

#fit a quadratic relationship
#equivalently, specify poly(temperature, 2, raw = TRUE)
quad_ols <- lm(rented_bike_count ~ temperature + I(temperature^2),
               data = bike_share)
quad_ols$coefficients

#fit an interaction relationship between temp and wind speed
interaction_ols <- lm(rented_bike_count ~ temperature*wind_speed,
                      data = bike_share)
interaction_ols$coefficients

#Fit including a qualitative variable
MLR_binary_pred <- lm(rented_bike_count ~ temperature + holiday,
                      data = bike_share)
MLR_binary_pred$coefficients

#Fit interaction between quantitative (temp) and qualitative (holiday) predictors
MLR_binary_interaction <- lm(rented_bike_count ~ temperature*holiday,
                             data = bike_share)
MLR_binary_interaction$coefficients

#fit with a interaction with qualitative variable with 4 levels (seasons)
MLR_qualitative_interaction <- lm(rented_bike_count ~ temperature*seasons,
                                  data = bike_share)
MLR_qualitative_interaction$coefficients

#Summary to see standard errors of coefficients
MLR_ols <- lm(rented_bike_count ~ temperature + wind_speed,
              data = bike_share)
summary(MLR_ols)

## 95\% confidence intervals 
ci <- confint(MLR_ols, level = 0.95)
ci

summary(MLR_binary_interaction)

# Reduced model with only intercept
MLR_red <- lm(rented_bike_count ~ 1, data = bike_share)
anova(MLR_red, MLR_binary_interaction)

#Check plots to investigate model assumptions
plot(MLR_binary_interaction)

#Residuals vs fitted plot shows a clear pattern -> model may not be accurate
#QQ plot shows deviation from line, normality may not hold
#sqrt(residuals) shows pattern, large residuals

#Use log transform to help assumptions become more reasonable
bike_share <- 
  bike_share |>
  mutate(log_rented_bike_count = log(rented_bike_count+1)) #add an offset for log(0) issues
MLR_log_int <- lm(log_rented_bike_count ~ temperature*holiday, data = bike_share)

summary(MLR_log_int)
plot(MLR_log_int)

#Residuals vs fitted plot shows much less pattern -> model should be more accurate
#QQ plot shows deviation toward the lower values
#No strong trend in sqrt(residuals), better

#We remove all observations where functioning day = no (no bikes rented)
#since those observations were causing many outliers, strange patterns
#in the diagnostic plots
bike_share <- bike_share |>
  filter(functioning_day != "No")

unique(bike_share$functioning_day)
#new SLR fit
SLR_fit <- lm(log_rented_bike_count ~ temperature, data = bike_share)
summary(SLR_fit)$coefficients

#predict a single new value
predict(SLR_fit, 
        newdata = data.frame(temperature = 22.22))

bike_share |>
  ggplot(aes(x = temperature, y = log_rented_bike_count)) +
  geom_point(size = 0.5) +
  geom_smooth(method = "lm")

#Construct prediction interval
predictions <- predict(SLR_fit,
                       newdata = bike_share,
                       interval="prediction")
predictions[1:4, 1:3]

#Add the predictions to the dataset
bike_share_preds <- cbind(bike_share, predictions)

bike_share |>
  ggplot(aes(x = temperature, y = log_rented_bike_count)) +
  geom_point(size = 0.5) +
  geom_smooth(method = "lm", se = FALSE) + 
  geom_line(data = bike_share_preds,
            aes(y = lwr), 
            color = "red", 
            linetype = "dashed") + 
  geom_line(data = bike_share_preds,
            aes(y = upr), 
            color = "red", 
            linetype = "dashed") 

conf_for_mean <- predict(SLR_fit, 
                         newdata = data.frame(temperature = 22.22), 
                         interval = "confidence")
conf_for_mean 
#95% confident that the mean for log rented bikes for all days with temp = 22.22
#is between 6.57 and 6.62

pred_for_future <- predict(SLR_fit,
                           data.frame(temperature = 22.22), 
                           interval = "prediction")
pred_for_future 
#95% confident that a future log rented bikes for 22.22 temp is between 4.72 and 8.47

#Perform 5-fold CV, repeated 10 times for more stability
library(caret)
set.seed(1001)
# control params
cv <- trainControl(method = "repeatedcv", 
                   number = 5, 
                   repeats = 10)
# training main effects
res_main <- train(log_rented_bike_count ~ hour + temperature + humidity + 
                    wind_speed + visibility + rainfall + 
                    snowfall + seasons + holiday, 
                  data = bike_share, 
                  method = "lm", 
                  trControl = cv)

#training interaction model
res_interaction <- train(log_rented_bike_count ~ (hour + temperature + wind_speed + rainfall +  snowfall + holiday)^2, 
                         data = bike_share, 
                         method = "lm", 
                         trControl = cv)

#training simpler model
res_simple <- train(log_rented_bike_count ~ hour + temperature + wind_speed + rainfall + seasons, 
                    data = bike_share, 
                    method = "lm", 
                    trControl = cv)

rbind(c("Main effect", round(res_main$results, 3)),
      c("Interaction", round(res_interaction$results, 3)),
      c("Simple", round(res_simple$results, 3))) 

#use best subset selection
library(leaps)
# Best model for each model size
bestmod <- regsubsets(log_rented_bike_count ~ hour + temperature + humidity + wind_speed + visibility + dew_point_temperature + solar_radiation + rainfall + snowfall + seasons + holiday, 
                      data = bike_share,
                      nvmax = 11)
# summary
mod_summary <- summary(bestmod)

#Evaluate subsets for AIC, BIC, adjusted R^2
metrics <- data.frame(aic = mod_summary$cp,
                      bic = mod_summary$bic,
                      adjR2 = mod_summary$adjr2)
metrics |>
  round(3)

#BIC best model
round(coef(bestmod, 9), 3) |>
  round(3) 

#AIC and adjusted R2 model
round(coef(bestmod, 10), 3) |>
  round(3)

#forward stepwise selection
forward <- regsubsets(log_rented_bike_count ~ hour + temperature + humidity + wind_speed + visibility + dew_point_temperature +  solar_radiation + rainfall + snowfall + seasons + holiday, 
                      data = bike_share,
                      nvmax = 11,
                      method = "forward")
mod_summary <- summary(forward)

#evaluate subsets
metrics <- data.frame(aic = mod_summary$cp,
                      bic = mod_summary$bic,
                      adjR2 = mod_summary$adjr2)

#BIC best model
round(coef(forward, 9), 3) |>
  round(3) 

#AIC and adjusted R2 model
round(coef(forward, 10), 3) |>
  round(3)

#backward stepwise selection
backward <- regsubsets(log_rented_bike_count ~ hour + temperature + humidity + wind_speed + visibility + dew_point_temperature +solar_radiation + rainfall + snowfall + seasons + holiday, 
                       data = bike_share,
                       nvmax = 11,
                       method = "backward")
# summary
mod_summary <- summary(backward)

metrics <- data.frame(aic = mod_summary$cp,
                      bic = mod_summary$bic,
                      adjR2 = mod_summary$adjr2)

#BIC best model
round(coef(backward, 9), 3) |>
  round(3) 

#AIC and adjusted R2 model
round(coef(backward, 10), 3) |>
  round(3)

#Use best subset with CV
set.seed(1001)
## Create test and training sets
data_split <- createDataPartition(bike_share$log_rented_bike_count, 
                                  p = 0.8, 
                                  list = FALSE)

test_set <- bike_share[-data_split, ]
train_set <- bike_share[data_split, ]

## Best subset selection on the training data
best_train <- regsubsets(log_rented_bike_count ~ hour + temperature + humidity + wind_speed + visibility + dew_point_temperature +solar_radiation + rainfall + snowfall + seasons + holiday,
                         data = train_set,
                         nvmax = 11)

train_sum <- summary(best_train)

#We'll write a function to predict and estimate the error on the test set. 
#Inputs are 
#- model size (mod_size),
#- summary output of the selection process (reg_summary)
#- model matrix of the test data (test_model)
#- test set response (test_resp)
test_err <- function(mod_size, 
                     reg_summary, 
                     test_model,
                     test_resp){
  # get regression coefs
  betahat <- coef(reg_summary$obj, mod_size)
  # get best subset of the specified size
  sub <- reg_summary$which[mod_size, ]
  # Create test model matrix, prediction, test error
  model <- test_model[, sub]
  yhat <- model %*% betahat
  err <- mean((test_resp - yhat)^2)
  return(err)
}

#define the test model
test_model <- model.matrix(~ hour + temperature + humidity + wind_speed + visibility + dew_point_temperature +solar_radiation + rainfall + snowfall + seasons + holiday,
                           data = test_set)

#define the test response
test_resp <- test_set$log_rented_bike_count

#apply the function to each of the model sizes
hold_err <- sapply(1:11, #apply the function to these 
                   FUN = test_err, 
                   reg_summary = train_sum,
                   test_model = test_model, 
                   test_resp = test_resp)

plot(hold_err, type = 'b', pch=19, lwd=2)

size_opt <- which.min(hold_err)
size_opt

#fit on the full data set
bestmod <- regsubsets(log_rented_bike_count ~ hour + temperature + humidity + wind_speed + visibility + dew_point_temperature +solar_radiation + rainfall + snowfall + seasons + holiday,
                      data = bike_share,
                      nvmax = 11)
#Use the optimal size
coef(bestmod, size_opt) |>
  round(3) 

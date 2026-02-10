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

SLR_fit <- lm(rented_bike_count ~ temperature, data = bike_share)
summary(SLR_fit)$coefficients


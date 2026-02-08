# Can run commands in the console for quick results
#Highlight a line to run it by itself (Ctrl Enter)

#Load a package for use in the file 
#Conflicts can happen if multiple packages have same function, 
#will use the function from the most recently loaded package
library(tidyverse)

#use ?function to get description of function/parameters/output
?stem()

#variables
avg <- (5 + 6 + 7)/3

#c = combine
words <- c("Hello There", "How are you?")

#print first n rows of data
head(cars, n = 3)

#str "structure" function to print details of data
str(cars)

#getwd shows the working directory
getwd()

#built in datasets dont automatically show up in environment tab, assign it to variable first
my_iris <- iris

#Most important data structures in R: Vector, Dataframe, List
#Vector: all elements must be same data type (int, double, etc)
x <- c(1, 3, 10, -20, sqrt(2))
y <- c("cat", "dog", "bird", "floor")
z <- c(x, y)

#element wise math by default
x + 3

#R starts counting at 1, not 0
letters
letters[1]

#indexing vector:
x <- c(1, 2, 5)
letters[x]

#Dataframe: 2D object (think pandas df)
#see select rows, columns (leave one blank to see all)
iris[1:4, 2:4]
iris[1, ]
iris[1:10, 1]

#List: 1D, but elements can be different data types
#lm function fits linear models
lm_fit <- lm(Sepal.Length ~ Sepal.Width, data = iris)
str(lm_fit)

#return an element, couple different ways, $ useful for previewing the elements beforehand
lm_fit[1]
lm_fit$coefficients

#tibble is special version of dataframe in tidyverse package
#:: grabs a function from a package without worrying about the rest of the package
iris_tbl <- dplyr::as_tibble(iris)
str(iris_tbl)

#main packages to use: ggplot2, dplyr, tidyr, readr

#read a csv file (?read_csv to review the many options it has)
bike_details <- read_csv("https://www4.stat.ncsu.edu/~online/datasets/bikeDetails.csv")

#Piping (|> = "then"), think select/filter/mutate as similar to SQL queries
iris |>
  select(Sepal.Length, starts_with("Petal")) |>
  filter(Sepal.Length > 5.4) |>
  mutate(Petal.Avg = (Petal.Length + Petal.Width)/2)

#Filter function, fetching rows where variables are constrained in some way, also reminiscent of SQL
library(Lahman)
batting_tbl <- as_tibble(Batting)
batting_tbl |> 
  filter(G > 50, yearID == 2018)

batting_tbl |>
  filter(G > 50, yearID %in% c(2018, 2019, 2020))

batting_tbl |>
  filter(G > 50 | yearID %in% c(2018, 2019, 2020))

#combine filter and select
batting_tbl |>
  filter(G > 50 | yearID %in% c(2018, 2019, 2020)) |>
  select(playerID, teamID, H, X2B, X3B, HR)

#combine filter and select
batting_tbl |>
  filter(G > 50 | yearID %in% c(2018, 2019, 2020)) |>
  select(ends_with("ID"), G, AB, H:HR)

#can rename columns that have weird characters
batting_tbl |>
  filter(G > 50 | yearID %in% c(2018, 2019, 2020)) |>
  select(playerID, teamID, H, X2B, X3B, HR) |>
  rename("Doubles" = "X2B", "Triples" = "X3B")

#mutate to create new variables, append to the dataset
batting_tbl |>
  filter(G > 50 | yearID %in% c(2018, 2019, 2020)) |>
  select(playerID, teamID, H, X2B, X3B, HR) |>
  rename("Doubles" = "X2B", "Triples" = "X3B") |>
  mutate(Extra_Base_Hits = Doubles + Triples + HR,
         Singles = H - Extra_Base_Hits) |>
  select(playerID, teamID, Singles, Doubles:HR, H, Extra_Base_Hits)

f <- function(n) {
  2 * (1 - pnorm(6.71 * sqrt(n) / sqrt(393 - n)))
}

# Create a sequence of n values (must be less than 393)
n_values <- seq(20, 100, by = 1)

# Calculate the function values
y_values <- f(n_values)

# Plot
plot(n_values, y_values, 
     type = "l",  # line plot
     xlab = "n",
     ylab = "2(1 - Φ(6.71√n/√(393-n)))",
     main = "Function Plot",
     col = "blue",
     lwd = 2)

# Add a grid for readability
grid()

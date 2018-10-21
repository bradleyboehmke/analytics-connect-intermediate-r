# packages reqiured
library(tidyverse)

######################
# control statements #
######################

# syntax of if statement
if (test_expression) {
  statement
}

x <- c(8, 3, -2, 5, NA)

if(x < 0) print("Negative")

if(any(x < 0)) print("Includes negative")

if(any(x < 0)) {
  print("x contains a negative number")
}

# syntax of if...else statement
if (test_expression) {
  statement 1
} else {
  statement 2
}

# if...else
if(any(x < 0)) {
  print("x contains negative number(s)")
} else{
  print("x contains all positive numbers")
}

# vectorized
ifelse(x < 0, "Negative", "Positive")

# extending if...else statements

x <- 7

if(x >= 10) {
  print("x exceeds acceptable tolerance levels")
} else if(x >= 0 & x < 10) {
  print("x is within acceptable tolerance levels")
} else {
  print("x is negative")
}

# Your Turn - Part 1
# create file name parts

file_1 <- "Month-"
file_2 <- ".csv"
month <- 13

if (month %in% 1:9) {
  paste0("data/", file_1, 0, month, file_2)
} else if (month %in% 10:12) {
  paste0("data/", file_1, month, file_2)
} else {
  print("Invalid month")
}

# Your Turn - Part 2
# search to see if file exists

file_1 <- "Month-"
file_2 <- ".csv"
month <- 12

if(month %in% 1:9) {
  file_name <- paste0("data/", file_1, 0, month, file_2)
  file.exists(file_name)
} else if(month %in% 10:12) {
  file_name <- paste0("data/", file_1, month, file_2)
  file.exists(file_name)
} else {
  print("Invalid month")
}

########################
# iteration with loops #
########################

# syntax of for loop
for(i in 1:100) {
  <do stuff here with i>
}

years <- 2010:2017

for (i in years) {
  output <- paste("The year is", i)
  print(output)
}

# output as vector
result <- vector(mode = "character", 
                 length = length(years))

result <- NULL

for (i in seq_along(years)) {
  output <- paste("The year is", years[i])
  result[i] <- output
}

result

# good point to talk about environment

# go back to our if...else statement

x <- 7

if(x >= 10) {
  print("x exceeds acceptable tolerance levels")
} else if(x >= 0 & x < 10) {
  print("x is within acceptable tolerance levels")
} else {
  print("x is negative")
}

x <- c(-1, 7, 8, 11)
tolerance <- vector(mode = "character", length = length(x))

for (i in seq_along(x)) {
  if(x[i] >= 10) {
    value <- "x exceeds acceptable tolerance levels"
  } else if(x[i] >= 0 & x[i] < 10) {
    value <- "x is within acceptable tolerance levels"
  } else {
    value <- "x is negative"
  }
  tolerance[i] <- value
}
tolerance

# Your Turn - part 1
# import each data set

file_1 <- "Month-"
file_2 <- ".csv"
month <- 1:13

for(i in month) {
  
  # create file name
  if(i %in% 1:9) {
    file_name <- paste0("data/", file_1, 0, i, file_2)
  } else if(i %in% 10:12) {
    file_name <- paste0("data/", file_1, i, file_2)
  } else {
    response <- paste(i, "is an invalid month")
    print(response)
    next
  }
  
  # import data
  if(file.exists(file_name)) {
    df <- read_csv(file_name)
    assign(paste0("df.month.", i), df)
    rm(df)
  } else {
    response <- paste("There is no available data for month", i)
    print(response)
  }
}


# how could we change this to just import into one single data frame?
file_1 <- "Month-"
file_2 <- ".csv"
month <- 1:13

# create empty data frame
df.all.months <- data.frame(NULL)

for(i in month) {
  
  # create file name
  if(i %in% 1:9) {
    file_name <- paste0("data/", file_1, 0, i, file_2)
  } else if(i %in% 10:12) {
    file_name <- paste0("data/", file_1, i, file_2)
  } else {
    response <- paste(i, "is an invalid month")
    print(response)
    next
  }
  
  # import data
  if(file.exists(file_name)) {
    df <- read_csv(file_name)
    df.all.months <- rbind(df.all.months, df)
    rm(df)
  } else {
    response <- paste("There is no available data for month", i)
    print(response)
  }
}

########################
# iteration with purrr #
########################
library(purrr)

# explain map_* functions
# Every map function works the same
# map_dbl(.x, .f, ...)
# 1. loop over a vector x
# 2. do something to each element .f
# 3. return the results
# 
# There is one function for each type of vector:
# map() returns a list
# map_dbl() returns a double vector 
# map_lgl() returns a logical vector 
# map_int() returns a integer vector 
# map_chr() returns a character vector

map_dbl(mtcars, mean)
mtcars %>% map_dbl(mean)
mtcars %>% map(mean)

# unlike the apply family, map functions use the same function to iterate over
# vectors, data frames, and lists

df <- data.frame(a = 1:10, b = 11:20)
df %>% map(mean)

l <- list(a = 1:10, b = 11:20)
l %>% map(mean)

vec <- c(1:10)
vec %>% map(mean)

# use map functions to answer these three questions
iris %>% map_chr(class)
iris %>% map_dbl(mean)

# option 1
iris %>%
  map_dbl(mean) %>%
  map_lgl(~ . > 5)

# option 2
iris %>% map_lgl(~ mean(.) > 5)


# specifying .f
map(df, summary)
map(df, myfunction)
map(df, function(x) sum(is.na(x)))
map(df, ~ sum(is.na(.)))

mtcars %>% map_dbl(~ sum(is.na(.)))

# what variables in the nycflights13::flights data have missing values?  How many?
nycflights13::flights %>% map_dbl(~ sum(is.na(.)))


# where does this become handy?
# for running multiple analyses

# i.e. lets split the mtcars data by different cyl
cyl <- split(mtcars, mtcars$cyl)
str(cyl)
cyl[[1]]

# let's say our goal was to fit regression to each of the data frame cyl and
# quantify and compare the relationships between mpg and wt

# option 1
mtcars %>% 
  split(.$cyl) %>% 
  map(function(df) lm(mpg ~ wt, data = df))

# option 2
mtcars %>% 
  split(.$cyl) %>% 
  map(~ lm(mpg ~ wt, data = .))

# this gives us a list of regression results for each cyl
# now lets access the summary results
mtcars %>% 
  split(.$cyl) %>% 
  map(~ lm(mpg ~ wt, data = .)) %>%
  map(summary)

# we can now extend this to get specific regression results
mtcars %>% 
  split(.$cyl) %>% 
  map(~ lm(mpg ~ wt, data = .)) %>%
  map(summary) %>%
  map_dbl("r.squared")

# this allows us to easily snag the slopes to compare
mtcars %>% 
  split(.$cyl) %>% 
  map(~ lm(mpg ~ wt, data = .)) %>%
  map(summary) %>%
  map("coefficients") %>%
  map_dbl(2)

# Your Turn!
# 1. split the diamonds data set by cut
# 2. run a regression on each list item lm(price ~ carat, data = .)
# 3. get the summary of the regression
# 4. how does the "r.squared" compare across models?
# 5. how do the model slopes compare?

ggplot2::diamonds %>% 
  split(.$cut) %>%
  map(~ lm(price ~ carat, data = .)) %>%
  map(summary) %>%
  map_dbl("r.squared")

ggplot2::diamonds %>% 
  split(.$cut) %>%
  map(~ lm(price ~ carat, data = .)) %>%
  map(summary) %>%
  map("coefficients") %>%
  map_dbl(2)

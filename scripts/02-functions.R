pv <- function(FV, r, n) {
  present_value <- FV / (1 + r)^n
  round(present_value, 2)
}

formals(pv)
body(pv)
environment(pv)

pv2 <- function(FV, r, n) {
  present_value <- FV / (1 + r)^n
  
  return(present_value)
  round(present_value, 2)
}

#############
# Your Turn #
#############
ratio <- function(x, y) {
  x / y
}

ratio(3, 4)


######################
# Handling arguments #
######################
# using argument names
pv(FV = 1000, r = .08, n = 5)

# same as above but without using names (aka "positional matching")
pv(1000, .08, 5)

# if using names you can change the order
pv(r = .08, FV = 1000, n = 5)

# if not using names you must insert arguments in proper order
# in this e.g. the function assumes FV = .08, r = 1000, and n = 5
pv(.08, 1000, 5)

# if missing arguments exist then we receive an error
pv(1000, .08)

# we can create default
pv <- function(FV, r, n = 5) {
  present_value <- FV / (1 + r)^n
  round(present_value, 2)
}

# now if we do not call n specifically the default is used
pv(1000, .08)

# and we can change n simply by calling it
pv(1000, .08, 3)

#############
# Your Turn #
#############
# Turn this into a function
(df$a - min(df$a, na.rm = TRUE)) /  
  (max(df$a, na.rm = TRUE) - min(df$a, na.rm = TRUE))

rescale <- function(x){
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
} 

# test it on this vector
set.seed(123)
vec1 <- runif(10, min = 5, max = 13)
rescale(vec1)

# Now add a rounding argument with the default to 2
rescale <- function(x, digits = 2){
  rng <- range(x, na.rm = TRUE)
  scaled <- (x - rng[1]) / (rng[2] - rng[1])
  round(scaled, digits = digits)
} 

# Now move na.rm = TRUE to formals
rescale <- function(x, digits = 2, na.rm = TRUE){
  if(isTRUE(na.rm)) x <- na.omit(x)
  rng <- range(x)
  scaled <- (x - rng[1]) / (rng[2] - rng[1])
  round(scaled, digits = digits)
} 

vec1 <- c(NA, vec1)
rescale(vec1)

# hiding missing values removed
rescale <- function(x, digits = 2, na.rm = TRUE){
  if(isTRUE(na.rm)) x <- x[!is.na(x)]
  rng <- range(x)
  scaled <- (x - rng[1]) / (rng[2] - rng[1])
  round(scaled, digits = digits)
}

######################
# Invalid Parameters #
######################
# test to make FV is an atomic vector
pv <- function(FV, r, n = 5) {
  
  if(!is.atomic(FV)) {
    stop('FV must be an atomic vector')
  }
  
  present_value <- FV / (1 + r)^n
  round(present_value, 2)
}

fv_l <- list(fv1 = 800, fv2 = 900, fv3 = 1100)
fv <- c(800, 900, 1100)

pv(fv_l, .08)
pv(fv, .08)

# now lets add tests for the type of class input
pv <- function(FV, r, n = 5) {
  
  if(!is.atomic(FV)) {
    stop('FV must be an atomic vector')
  }
  
  if(!is.numeric(FV) | !is.numeric(r) | !is.numeric(n)){
    stop('This function only works for numeric inputs!\n', 
         'You have provided objects of the following classes:\n', 
         'FV: ', class(FV), '\n',
         'r: ', class(r), '\n',
         'n: ', class(n))
  }
  
  present_value <- FV / (1 + r)^n
  round(present_value, 2)
}

pv(FV = "1000", .08, n = 5)


# what about providing a message for irregular interest rate ranges
pv <- function(FV, r, n = 5) {
  
  if(!is.atomic(FV)) {
    stop('FV must be an atomic vector')
  }
  
  if(!is.numeric(FV) | !is.numeric(r) | !is.numeric(n)){
    stop('This function only works for numeric inputs!\n', 
         'You have provided objects of the following classes:\n', 
         'FV: ', class(FV), '\n',
         'r: ', class(r), '\n',
         'n: ', class(n))
  }
  
  if(r < 0 | r > .25) {
    message('The input for r exceeds the normal\n',
            'range for interest rates (0-25%)')
  }
  
  present_value <- FV / (1 + r)^n
  round(present_value, 2)
}

pv(FV = 1000, .28, n = 5)


#############
# Your Turn #
#############
# add an if statement and stop function for:
# - making sure x input is a numeric vector
# - digits input is a numeric vector of one element
# - na.rm input is a single logical input
rescale <- function(x, digits = 2, na.rm = TRUE){
  if(isTRUE(na.rm)) x <- x[!is.na(x)]
  rng <- range(x)
  scaled <- (x - rng[1]) / (rng[2] - rng[1])
  round(scaled, digits = digits)
}

rescale <- function(x, digits = 2, na.rm = TRUE){
  # ensure argument inputs are valid
  if(!is.numeric(x)) {
    stop('x must be an atomic numeric vector')
  }
  if(!is.numeric(digits) | length(digits) > 1) {
    stop('digits must be a numeric vector of one element')
  }
  if(!is.logical(na.rm)) {
    stop('na.rm must be logical input (TRUE or FALSE)')
  }
  
  if(isTRUE(na.rm)) x <- x[!is.na(x)]
  rng <- range(x)
  scaled <- (x - rng[1]) / (rng[2] - rng[1])
  round(scaled, digits = digits)
}

rescale(c(letters))
rescale(vec1, digits = c(1, 2))
rescale(vec1, na.rm = "false")


###############
# Other Notes #
###############
# lazy evaluation
lazy <- function(x, y = NULL) {
  if(!is.null(y)) {
    return(x * 2 + y)
  }
  x * 2
}
lazy(4)
lazy(4, 1)

# scoping rules
y <- 2
scoping <- function(x) {
  if(!is.null(y)) {
    return(x * 2 + y)
  }
  x * 2
}
scoping(4)


##############################
# PRACTICE WRITING FUNCTIONS #
##############################
set.seed(123)
x <- rlnorm(100)

# create a function "variance" that computes the variance of x
(1 / (length(x) - 1)) * sum((x - mean(x))^2)

variance <- function(x) {
  n <- length(x)
  m <- mean(x)
  (1/(n - 1)) * sum((x - m)^2)
}

# create a function "std_dev" that computes the standard deviation of x
sqrt((1 / (length(x) - 1)) * sum((x - mean(x))^2))

std_dev <- function(x) {
  sqrt(variance(x))
}


# create a function "std_error" that computes the standard error of x
var_x <- (1 / (length(x) - 1)) * sum((x - mean(x))^2)
sqrt(var_x / length(x))

std_error <- function(x) {
  n <- length(x)
  sqrt(variance(x) / n)
}

# create a function "skewness" that computes the skewness of x
n <- length(x)
v <- var(x)
m <- mean(x)
third.moment <- (1 / (n - 2)) * sum((x - m)^3)
third.moment / (var(x)^(3 / 2))

skewness <- function(x) {
  n <- length(x)
  v <- variance(x)
  m <- mean(x)
  third.moment <- (1 / (n - 2)) * sum((x - m)^3)
  third.moment / (v^(3 / 2))
}

# now apply these functions over the mtcars variables with the map functions
library(tidyverse)
mtcars %>% 
  map_df(skewness)




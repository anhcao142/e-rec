test <- read.csv("300 items/POPULAR.csv", header = TRUE)
test <- read.csv("300 items/UBCF_jacard.csv", header = TRUE)
test <- read.csv("300 items/POPULAR.csv", header = TRUE)
test <- read.csv("300 items/POPULAR.csv", header = TRUE)



# Function that returns Root Mean Squared Error
rmse <- function(error)
{
  sqrt(mean(error^2))
}

# Function that returns Mean Absolute Error
mae <- function(error)
{
  mean(abs(error))
}

# Example data
names(test) <- c("user", "item", "real_rating", "rating")
actual <- test$real_rating
predicted <- test$rating

# Calculate error
error <- actual - predicted

# Example of invocation of functions
rmse(error)
mae(error)
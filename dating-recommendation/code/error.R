#test1 <- read.csv("POPULAR.csv", header = TRUE)
#test2 <- read.csv("UBCF_jacard.csv", header = TRUE)
#test3 <- read.csv("IBCF_jacard.csv", header = TRUE)
#test4 <- read.csv("UBCF_COSINE.csv", header = TRUE)
test5 <- read.csv("BASELINE.csv", header = TRUE)
test6 <- read.csv("BELLKOV.csv", header = TRUE)



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


run <- function(test) {
  # Example data
  names(test) <- c("user", "item", "real_rating", "rating")
  actual <- test$real_rating
  predicted <- test$rating
  
  # Calculate error
  error <- actual - predicted
  
  # Example of invocation of functions
  print(rmse(error))
  print(mae(error))
  return(list(rmse = rmse(error), mae = mae(error)))
}

test0 <- read.csv("0_UBCF_COSINE.csv", header = TRUE)
test1 <- read.csv("1_UBCF_COSINE.csv", header = TRUE)
test2 <- read.csv("2_UBCF_COSINE.csv", header = TRUE)
test3 <- read.csv("3_UBCF_COSINE.csv", header = TRUE)
test4 <- read.csv("4_UBCF_COSINE.csv", header = TRUE)
test5 <- read.csv("5_UBCF_COSINE.csv", header = TRUE)
run(test0)
run(test1)
run(test2)
run(test3)
run(test4)
run(test5)


test0 <- read.csv("0_IBCF_COSINE.csv", header = TRUE)
test1 <- read.csv("1_IBCF_COSINE.csv", header = TRUE)
test2 <- read.csv("2_IBCF_COSINE.csv", header = TRUE)
test3 <- read.csv("3_IBCF_COSINE.csv", header = TRUE)
test4 <- read.csv("4_IBCF_COSINE.csv", header = TRUE)
test5 <- read.csv("5_IBCF_COSINE.csv", header = TRUE)
run(test0)
run(test1)
run(test2)
run(test3)
run(test4)
run(test5)
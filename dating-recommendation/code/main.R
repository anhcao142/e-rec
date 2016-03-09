#setwd("~/e-rec/dating-recommendation/test_number_item/14-20")


#tr <- read.csv('train.csv', header = TRUE)
#test<-read.csv("test.csv",header=TRUE)
num = 20
for(i in 1 : 20) {
  data_file <- paste(i*5, '.csv', sep = "")
  data <- read.csv(data_file, header = TRUE)
  set.seed(7)
  ss <- sample(1:2,size=nrow(data),replace=TRUE,prob=c(0.9,0.1))
  tr <- data[ss==1,]
  test <- data[ss==2,]
  
  l1 = 25
  l2 = 10
  g<-acast(tr, user ~ item)
  uID <- row.names(g)
  average <- mean(g, na.rm=TRUE)
  
  file_path <- "BASELINE.csv"
  rec_list <- baseline_prediction(file_path)
  
  file_path <- "BELLKOV.csv"
  rec_list <- bellkov_prediction(file_path)
  
  test5 <- read.csv("BASELINE.csv", header = TRUE)
  test6 <- read.csv("BELLKOV.csv", header = TRUE)
  
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
  
  r5 <- run(test5)
  r6 <- run(test6)
  
  mae <- data.frame(c(r5$mae, r6$mae))
  names(mae) <- c(i*num)
  rmse <- data.frame(c(r5$rmse, r6$rmse))
  names(rmse) <- c(i*num)
  write.table(mae,"result/MAE.csv", row.names=F,na="NA",append=T, quote= FALSE, sep=",", col.names=T)
  write.table(rmse,"result/RMSE.csv", row.names=F,na="NA",append=T, quote= FALSE, sep=",", col.names=T)
  print(i)
  flush.console()
}
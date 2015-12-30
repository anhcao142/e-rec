library(randomForest)
library(foreach)
#library(Hmisc)
#library(doMC)
library(rminer)
library(doSNOW)

rf <- function(test.set, train.set) {
  cl <- makeCluster(2, type="SOCK")
  registerDoSNOW(cl)
  rmse <- function(obs, pred) {return(sqrt(mean((as.numeric(obs)-as.numeric(pred))^2)))}
  #{imputation (knn-3), na.roughfix, imputate}
  imputation.method <- function(data){return (imputation(imethod='hotdeck',data,Value=3))}
  
  train.set <- imputation.method(train.set)
  test.set <- imputation.method(test.set)
  features <- -grep("^*(userID|eventID)$", names(train.set)) #exclude non-use features
  
  ## as a regression, because interest_rank now is int 
  train.set_validation.index <- sample(nrow(train.set), nrow(train.set)/5, replace = F)
  train.set_train <- train.set[-train.set_validation.index, features]
  train.set_validation <- train.set[train.set_validation.index, features]
  
  ## build randomForest model in parallel
  ## use whole set to train
  rf.model <- foreach(ntree=rep(400,8), .combine=combine, .packages="randomForest", .inorder=F) %dopar% {
    randomForest(status~., data=train.set[,features], 
                 ntree=ntree, importance=T, na.action=na.roughfix)
  }
  ## validate the built model
  print (rmse(train.set_validation$status, 
              predict(rf.model, newdata=train.set_validation, type='response')))
  ## predict on test data
  test.prediction <- predict(rf.model, newdata=test.set, type='response')
  test.set <- transform(test.set, status=test.prediction)
  
  return(test.set)
}

test.set <- read.csv('29-12/test_word_score.csv', header=T)
str(test.set)
train.set <- read.csv('29-12/train_word_score.csv', header=T)
str(train.set)

test <- rf(test.set, train.set)
## write test to csv
write.csv(test, '29-12/test_word_predict.csv', row.names=F)

test.set <- read.csv('29-12/test_time_score.csv', header=T)
str(test.set)
train.set <- read.csv('29-12/train_time_score.csv', header=T)
str(train.set)

test <- rf(test.set, train.set)
## write test to csv
write.csv(test, '29-12/test_time_predict.csv', row.names=F)

test.set <- read.csv('29-12/test_location_score.csv', header=T)
str(test.set)
train.set <- read.csv('29-12/train_location_score.csv', header=T)
str(train.set)

test <- rf(test.set, train.set)
## write test to csv
write.csv(test, '29-12/test_location_predict.csv', row.names=F)


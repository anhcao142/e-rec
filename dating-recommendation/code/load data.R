#setwd("~/e-rec/dating-recommendation")
library(recommenderlab)
library(reshape2)
library(ggplot2)


prepare_data <- function() {
  #readLines("libimseti/ratings.dat",  n=10)
  data <- read.table("ratings.dat",header=FALSE, sep = ",")
  names(data) <- c("user", "item", "rating")
  write.csv(data, "100000.csv", row.names=F)
  data <- read.csv("100000.csv", header = TRUE)
  item <- data$item
  a <- as.data.frame(table(item))
  a <- a[order(a$Freq, decreasing = TRUE),]
  write.csv(a, "item.csv", row.names = FALSE)
  item <- a$item[17900:18200]
  item <- as.numeric(levels(item))[item]
  new_data <- matrix(data = NA, nrow = 0, ncol = 3)
  for(i in 1: nrow(data)) {
    if(!is.na(match(data$item[i],item))) {
      new_data <- rbind(new_data, data[i,])
    }
  }
  new_data <- data.frame(new_data)
  names(new_data) <- c("user", "item", "rating")
  write.csv(new_data, 'data.csv', row.names=F)
  
  data <- read.csv("data.csv", header = TRUE)
  set.seed(7)
  ss <- sample(1:2,size=nrow(data),replace=TRUE,prob=c(0.999,0.001))
  train <- data[ss==1,]
  test <- data[ss==2,]
  write.csv(train, 'train.csv', row.names=F)
  write.csv(test, 'test.csv', row.names=F)
  for(i in 1: nrow(test)) {
    if(is.na(match(test$user[i],train$user))) {
      print(nrow(test))
      flush.console()
      test <- test[-c(i),]
      i <- i-1
    }
  }
  
  write.csv(test, 'test.csv', row.names=F)
}

create_data <- function(index1, index2) {
  data <- read.csv("100000.csv", header = TRUE)
  a <- read.csv("item.csv", header = TRUE)
  item <- a$item[index1:index2]
  new_data <- matrix(data = NA, nrow = 0, ncol = 3)
  for(i in 1: nrow(data)) {
    if(!is.na(match(data$item[i],item))) {
      new_data <- rbind(new_data, data[i,])
    }
  }
  new_data <- data.frame(new_data)
  names(new_data) <- c("user", "item", "rating")
  write.csv(new_data, 'data.csv', row.names=F)
  
  data <- read.csv("data.csv", header = TRUE)
  data <- read.csv("ratings.dat", header = TRUE)
  names(data) <- c("user", "item", "rating")
  set.seed(7)
  ss <- sample(1:2,size=nrow(data),replace=TRUE,prob=c(0.9,0.1))
  train <- data[ss==1,]
  test <- data[ss==2,]
  write.csv(train, 'train.csv', row.names=F)
  write.csv(test, 'test.csv', row.names=F)
  for(i in 1: nrow(test)) {
    if(is.na(match(test$user[i],train$user))) {
      print(nrow(test))
      flush.console()
      test <- test[-c(i),]
      i <- i-1
    }
  }
  
  write.csv(test, 'test.csv', row.names=F)
}

#create_data(1686, 1389)
tr <- read.csv('train.csv', header = TRUE)
test<-read.csv("test.csv",header=TRUE)

# Using acast to convert above data as follows:
#       m1  m2   m3   m4
# u1    3   4    2    5
# u2    1   6    5
# u3    4   4    2    5
g<-acast(tr, user ~ item)
uID <- row.names(g)
# Convert it as a matrix
R<-as.matrix(g)

# Convert R into realRatingMatrix data structure
#   realRatingMatrix is a recommenderlab sparse-matrix like data-structure
r <- as(R, "realRatingMatrix")

collaborative <- function(rec, file_path) {
  # This prediction does not predict movie ratings for test.
  #   But it fills up the user 'X' item matrix so that
  #    for any userid and movieid, I can find predicted rating
  #     dim(r) shows there are 6040 users (rows)
  #      'type' parameter decides whether you want ratings or top-n items
  #         get top-10 recommendations for a user, as:
  #             predict(rec, r[1:nrow(r)], type="topNList", n=10)
  recom <- predict(rec, r[1:nrow(r)], type="ratings")
  
  print("train DONE")
  flush.console()
  
  # Get ratings list
  rec_list<-as(recom,"list")
  head(summary(rec_list))
  ratings<-NULL
  # For all lines in test file, one by one
  for ( u in 1:length(test[,2]))
  {
    # Read userid and movieid from columns 2 and 3 of test data
    userid <- test[u,1]
    movieid<-test[u,2]
    
    # Get as list & then convert to data frame all recommendations for user: userid
    index <- match(userid, uID)
    u1<-as.data.frame(rec_list[[index]])
    # Create a (second column) column-id in the data-frame u1 and populate it with row-names
    # Remember (or check) that rownames of u1 contain are by movie-ids
    # We use row.names() function
    u1$id<-row.names(u1)
    # Now access movie ratings in column 1 of u1
    x= u1[u1$id==movieid,1]
    # print(u)
    # print(length(x))
    # If no ratings were found, assign 0. You could also
    #   assign user-average
    if (length(x)==0)
    {
      ratings[u] <- 0
    }
    else
    {
      ratings[u] <-x
    }
    
  }
  length(ratings)
  tx<-cbind(test,round(ratings))
  # Write to a csv file: submitfile.csv in your folder
  write.table(tx,file=file_path,row.names=FALSE,col.names=FALSE,sep=',')
  return(rec_list)
}



#rec1=Recommender(r[1:nrow(r)],method="UBCF", param=list(normalize = "Z-score",method="Cosine",nn=5, minRating=1))
#rec2=Recommender(r[1:nrow(r)],method="UBCF", param=list(normalize = "Z-score",method="Jaccard",nn=5, minRating=1))
#rec3=Recommender(r[1:nrow(r)],method="IBCF", param=list(normalize = "Z-score",method="Jaccard",minRating=1))
rec4=Recommender(r[1:nrow(r)],method="POPULAR")
#file_path <- "UBCF_COSINE.csv"
#rec_list <- collaborative(rec1, file_path)

#file_path <- "UBCF_jacard.csv"
#rec_list <- collaborative(rec2, file_path)

#file_path <- "IBCF_jacard.csv"
#rec_list <- collaborative(rec3, file_path)

file_path <- "POPULAR.csv"
rec_list <- collaborative(rec4, file_path)



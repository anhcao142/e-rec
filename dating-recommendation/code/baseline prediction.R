#setwd("~/e-rec/dating-recommendation")
#setwd("~/e-rec/dating-recommendation/test_number_item/1-2")
library(recommenderlab)
library(reshape2)
#library(irlba)
#library(svd)

l1 = 25
l2 = 10
tr <- read.csv('train.csv', header = TRUE)
test<-read.csv("test.csv",header=TRUE)

# Using acast to convert above data as follows:
#       m1  m2   m3   m4
# u1    3   4    2    5
# u2    1   6    5
# u3    4   4    2    5
g<-acast(tr, user ~ item)
uID <- row.names(g)
average <- mean(g, na.rm=TRUE)

baseline_sim <- function(average, userid, itemid){
  if(is.na(match(userid, rownames(g)))) {
    u <- 0
  } else {
    bu <- g[as.character(userid),]
    u <- mean(bu, na.rm=TRUE) - average
  }
  if(is.na(match(itemid, colnames(g)))) {
    i <- 0
  } else {
    bi <- g[,as.character(itemid)]
    i <- mean(bi, na.rm=TRUE) - average
  }
  return(average + u + i)
}

baseline_prediction <- function(file_path) {
  # For all lines in test file, one by one
  ratings<-NULL
  for ( u in 1:length(test[,2]))
  {
    # Read userid and movieid from columns 2 and 3 of test data
    userid <- test[u,1]
    itemid<-test[u,2]
    #print(u)
    #flush.console()
    
    # Get as list & then convert to data frame all recommendations for user: userid
    r <- baseline_sim(average, userid, userid)
    if(is.na(r)) {
      ratings[u] <- 0
    } else {
      ratings[u] <- r
    }
    #print(r)
    #flush.console()
  }
  tx<-cbind(test,round(ratings))
  # Write to a csv file: submitfile.csv in your folder
  write.table(tx,file=file_path,row.names=FALSE,col.names=FALSE,sep=',')
}


bias_item <- function(average, itemid) {
  if(is.na(match(itemid, colnames(g)))) {
    return(-average/l1)
  }
  i <- g[,as.character(itemid)]
  i <- i[!is.na(i)]
  return(sum(i-average)/(l1+length(i)))
} 

bias_user <- function(average, userid) {
  if(is.na(match(userid, rownames(g)))) {
    return(-average/l2)
  }
  u <- g[as.character(userid),]
  u <- u[!is.na(u)]
  bi <- 0
  for(i in 1: length(u)) {
    bi <- bias_item(average, names(u)[i]) + bi
  }
  
  return((sum(u-average)-bi)/(l2+length(u)))
  
}

bellkov_sim <- function(average, userid, itemid) {
  bi = bias_item(average, itemid)
  bu = bias_user(average, userid)
  return(average + bi + bu)
}

bellkov_prediction <- function(file_path) {
  # For all lines in test file, one by one
  ratings<-NULL
  #length(test[,2])
  for ( u in 1:length(test[,2]))
  {
    # Read userid and movieid from columns 2 and 3 of test data
    userid <- test[u,1]
    itemid<-test[u,2]
    # Get as list & then convert to data frame all recommendations for user: userid
    r <- bellkov_sim(average, userid, itemid)
    if(is.na(r)) {
      ratings[u] <- 0
    } else {
      ratings[u] <- r
    }
    #print(u)
    #flush.console()
    #print(r)
    #flush.console()
  }
  tx<-cbind(test,ratings)
  # Write to a csv file: submitfile.csv in your folder
  write.table(tx,file=file_path,row.names=FALSE,col.names=FALSE,sep=',')
}


file_path <- "BASELINE.csv"
rec_list <- baseline_prediction(file_path)

file_path <- "BELLKOV.csv"
rec_list <- bellkov_prediction(file_path)
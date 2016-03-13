library(recommenderlab)
library(reshape2)


tr <- read.csv('train_v2.csv', header = TRUE)
tr$ID <- NULL
names(tr) <- c("user", "item", "rating")
test<-read.csv("test_v2.csv",header=TRUE)


l1 = 25
l2 = 10
g<-acast(tr, user ~ item)
#uID <- row.names(g)
average <- mean(g, na.rm=TRUE)

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
  return(average + bias_user(average, userid) + bias_item(average, itemid))
}

bellkov_prediction <- function(file_path) {
  # For all lines in test file, one by one
  
  for ( u in 202017:length(test[,3]))
  {
    rating<-NULL
    # Read userid and movieid from columns 2 and 3 of test data
    userid <- test[u,2]
    itemid<-test[u,3]
    
    # Get as list & then convert to data frame all recommendations for user: userid
    r <- bellkov_sim(average, userid, itemid)
    if(is.na(r)) {
      rating <- 0
    } else {
      rating <- r
    }
    print(u)
    flush.console()
    print(r)
    flush.console()
    tx<-cbind(test$ID[u],rating)
    # Write to a csv file: submitfile.csv in your folder
    write.table(tx,file=file_path,append=T,quote= FALSE,row.names=FALSE,col.names=FALSE,sep=',')
  }
}


file_path <- "submission.csv"
rec_list <- bellkov_prediction(file_path)

t <- read.csv("submission.csv", header= TRUE)

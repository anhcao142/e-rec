l1 = 25
l2 = 10
tr <- read.csv('train.csv', header = TRUE)
tr <- tr[order(tr$user),]
test<-read.csv("test.csv",header=TRUE)


average <- mean(tr$rating, na.rm=TRUE)


item_bias <- function() {
  sum <- tapply(tr$rating, tr$item, FUN = sum)
  item <- tr$item
  count <- as.data.frame(table(item))
  count <- count[order(count$item, decreasing = FALSE),]
  count$item <- as.numeric(levels(count$item))[count$item]
  bi <- NULL
  for(i in 1: length(count$item)) {
    bi[i] <- (sum[i] - average*count$Freq[i])/(l1 + count$Freq[i])
    print(i)
    flush.console()
  }
  tx<-cbind(count$item,bi)
  colnames(tx) <- c("item", "bias")
  # Write to a csv file: submitfile.csv in your folder
  write.table(tx,file="item bias.csv",row.names=FALSE,col.names=TRUE,sep=',')
}

prepare_user <- function () {
  user <- test$user[!duplicated(test$user)]
  toBeKeeped <- which(tr$user %in% user)
  tr <- tr[toBeKeeped,]
  write.table(tr,file="new train 1000.csv",row.names=FALSE,col.names=TRUE,sep=',')
}
user_bias <- function() {
  item_bias <- read.csv('item bias.csv', header = TRUE)
  user <- tr$user
  count <- as.data.frame(table(user))
  count$user <- as.numeric(levels(count$user))[count$user]
  index <- 1
  bui <- 0
  bu <- NULL
  #
  for(i in 1: length(count$user)) {
    for(j in 1: count$Freq[i]) {
      id <- match(tr$item[index], item_bias$item)
      bui <- tr$rating[index] - average - item_bias$bias[id] + bui
      print(c(tr$item[index], tr$rating[index], item_bias$bias[id], bui))
      index <- index+1
    }
    bu[i] <- bui/(l2 + count$Freq[i])
    print(bu[i])
    print(i)
    flush.console()
    bui <- 0
  }
  tx<-cbind(count$user ,bu)
  colnames(tx) <- c("user", "bias")
  # Write to a csv file: submitfile.csv in your folder
  write.table(tx,file="user bias.csv",row.names=FALSE,col.names=TRUE,sep=',')
}
item_bias()
user_bias()


item_bias <- read.csv('item bias.csv', header = TRUE)
user_bias <- read.csv('user bias.csv', header = TRUE)

bias_item <- function(average, itemid) {
  index <- match(itemid, item_bias$item)
  if(is.na(index)) {
    return(-average/l1)
  }
  return(item_bias$bias[index])
} 

bias_user <- function(average, userid) {
  index <- match(userid, user_bias$user)
  if(is.na(index)) {
    return(-average/l2)
  }
  return(user_bias$bias[index])
  
}

bellkov_sim <- function(average, userid, itemid) {
  bi = bias_item(average, itemid)
  bu = bias_user(average, userid)
  print(c(itemid, userid))
  flush.console()
  print(c(bi, bu))
  flush.console()
  return(average + bi + bu)
}

bellkov_prediction <- function(file_path) {
  # For all lines in test file, one by one
  ratings<-NULL
  #length(test[,2])
  for ( u in 1:length(test[,2]))
  {
    print(u)
    flush.console()
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
  }
  tx<-cbind(test,round(ratings))
  # Write to a csv file: submitfile.csv in your folder
  write.table(tx,file=file_path,row.names=FALSE,col.names=FALSE,sep=',')
}

file_path <- "BELLKOV.csv"
rec_list <- bellkov_prediction(file_path)
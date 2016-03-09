library(svd)
library(recommenderlab)
library(reshape2)
library(optimbase)
library(OpenMx)

tr <- read.csv('train.csv', header = TRUE)
test<-read.csv("test.csv",header=TRUE)

# Using acast to convert above data as follows:
#       m1  m2   m3   m4
# u1    3   4    2    5
# u2    1   6    5
# u3    4   4    2    5
g<-acast(tr, user ~ item)
g[is.na(g)] <- 0
s <- svd(g)
u <- as.matrix(s$u)
d <- vec2diag(s$d)
v<- as.matrix(s$v)

svd_sim <- function(userid, itemid) {
  if(is.na(match(userid, rownames(g))) || is.na(match(itemid, colnames(g)))) {
    return(0)
  }
  user <- g[as.character(userid),]
  item <- g[, as.character(itemid)]
  average <- mean(g[user, ], na.rm=TRUE)
  t <- sqrt(d)
  bu <- u %*% transpose(t)
  bi <- t %*% transpose(v)
  return(average + bu[user, ]%*%bi[item,])
}
svd_prediction <- function(file_path) {
  ratings<-NULL
  for ( u in 1:length(test[,2]))
  {
    # Read userid and movieid from columns 2 and 3 of test data
    userid <- test[u,1]
    itemid<-test[u,2]
    
    # Get as list & then convert to data frame all recommendations for user: userid
    r <- svd_sim(userid, itemid)
    if(is.na(r)) {
      ratings[u] <- 0
    } else {
      ratings[u] <- r
    }
    print(u)
    flush.console()
    print(r)
    flush.console()
  }
  tx<-cbind(test,round(ratings))
  # Write to a csv file: submitfile.csv in your folder
  write.table(tx,file=file_path,row.names=FALSE,col.names=FALSE,sep=',')
}

file_path <- "SVD.csv"
rec_list <- svd_prediction(file_path)
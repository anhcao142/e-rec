setwd("~/Incremental/20 000")

test = read.table("20 000_test.txt", sep="\t", fill=FALSE, strip.white=TRUE)

setwd("~/Incremental/20 000/500")
d0 = read.table("20 000_0.txt", sep="\t", fill=FALSE, strip.white=TRUE)
d0 <- d0[-which(d0$V3 == 0), ] 
d1 = read.table("20 000_1.txt", sep="\t", fill=FALSE, strip.white=TRUE)
d1 <- d1[-which(d1$V3 == 0), ] 
d2 = read.table("20 000_2.txt", sep="\t", fill=FALSE, strip.white=TRUE)
d2 <- d2[-which(d2$V3 == 0), ] 
d3 = read.table("20 000_3.txt", sep="\t", fill=FALSE, strip.white=TRUE)
d3 <- d3[-which(d3$V3 == 0), ] 
d4 = read.table("20 000_4.txt", sep="\t", fill=FALSE, strip.white=TRUE)
d4 <- d4[-which(d4$V3 == 0), ] 
d5 = read.table("20 000_5.txt", sep="\t", fill=FALSE, strip.white=TRUE)
d5 <- d5[-which(d5$V3 == 0), ] 

names(d0) <- c("user", "item", "rating")
names(d1) <- c("user", "item", "rating")
names(d2) <- c("user", "item", "rating")
names(d3) <- c("user", "item", "rating")
names(d4) <- c("user", "item", "rating")
names(d5) <- c("user", "item", "rating")
names(test) <- c("user", "item", "rating")
t1 <- rbind(d0, d1)
t2 <- rbind(t1, d2)
t3 <- rbind(t2, d3)
t4 <- rbind(t3, d4)
t5 <- rbind(t4, d5)

write.csv(d0, '0.csv', row.names=F)
write.csv(t1, '1.csv', row.names=F)
write.csv(t2, '2.csv', row.names=F)
write.csv(t3, '3.csv', row.names=F)
write.csv(t4, '4.csv', row.names=F)
write.csv(t5, '5.csv', row.names=F)
write.csv(test, 'test.csv', row.names=F)


library(recommenderlab)
library(reshape2)

collaborative <- function(rec, file_path) {
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
  tx<-cbind(test,ratings)
  # Write to a csv file: submitfile.csv in your folder
  write.table(tx,file=file_path,row.names=FALSE,col.names=FALSE,sep=',')
  return(rec_list)
}



test<-read.csv("test.csv",header=TRUE)

tr <- read.csv('0.csv', header = TRUE)
g<-acast(tr, user ~ item)
uID <- row.names(g)
# Convert it as a matrix
R<-as.matrix(g)
r <- as(R, "realRatingMatrix")
rec1=Recommender(r[1:nrow(r)],method="UBCF", param=list(normalize = "Z-score",method="Cosine",nn=100, minRating=1))
#rec1=Recommender(r[1:nrow(r)],method="IBCF", param=list(normalize = "Z-score",method="Cosine",minRating=1))
file_path <- "0_UBCF_COSINE.csv"
rec_list <- collaborative(rec1, file_path)

tr <- read.csv('1.csv', header = TRUE)
g<-acast(tr, user ~ item)
uID <- row.names(g)
# Convert it as a matrix
R<-as.matrix(g)
r <- as(R, "realRatingMatrix")
#rec1=Recommender(r[1:nrow(r)],method="UBCF", param=list(normalize = "Z-score",method="Cosine",nn=5, minRating=1))
rec1=Recommender(r[1:nrow(r)],method="IBCF", param=list(normalize = "Z-score",method="Cosine",minRating=1))
file_path <- "1_IBCF_COSINE.csv"
rec_list <- collaborative(rec1, file_path)

tr <- read.csv('2.csv', header = TRUE)
g<-acast(tr, user ~ item)
uID <- row.names(g)
# Convert it as a matrix
R<-as.matrix(g)
r <- as(R, "realRatingMatrix")
#rec1=Recommender(r[1:nrow(r)],method="UBCF", param=list(normalize = "Z-score",method="Cosine",nn=5, minRating=1))
rec1=Recommender(r[1:nrow(r)],method="IBCF", param=list(normalize = "Z-score",method="Cosine",minRating=1))
file_path <- "2_IBCF_COSINE.csv"
rec_list <- collaborative(rec1, file_path)


tr <- read.csv('3.csv', header = TRUE)
g<-acast(tr, user ~ item)
uID <- row.names(g)
R<-as.matrix(g)
r <- as(R, "realRatingMatrix")
#rec1=Recommender(r[1:nrow(r)],method="UBCF", param=list(normalize = "Z-score",method="Cosine",nn=5, minRating=1))
rec1=Recommender(r[1:nrow(r)],method="IBCF", param=list(normalize = "Z-score",method="Cosine",minRating=1))
file_path <- "3_IBCF_COSINE.csv"
rec_list <- collaborative(rec1, file_path)

tr <- read.csv('4.csv', header = TRUE)
g<-acast(tr, user ~ item)
uID <- row.names(g)
R<-as.matrix(g)
r <- as(R, "realRatingMatrix")
#rec1=Recommender(r[1:nrow(r)],method="UBCF", param=list(normalize = "Z-score",method="Cosine",nn=5, minRating=1))
rec1=Recommender(r[1:nrow(r)],method="IBCF", param=list(normalize = "Z-score",method="Cosine",minRating=1))
file_path <- "4_IBCF_COSINE.csv"
rec_list <- collaborative(rec1, file_path)

tr <- read.csv('5.csv', header = TRUE)
g<-acast(tr, user ~ item)
uID <- row.names(g)
R<-as.matrix(g)
r <- as(R, "realRatingMatrix")
#rec1=Recommender(r[1:nrow(r)],method="UBCF", param=list(normalize = "Z-score",method="Cosine",nn=5, minRating=1))
rec1=Recommender(r[1:nrow(r)],method="IBCF", param=list(normalize = "Z-score",method="Cosine",nn=100,minRating=1))
file_path <- "5_IBCF_COSINE.csv"
rec_list <- collaborative(rec1, file_path)

#file_path <- "UBCF_jacard.csv"
#rec_list <- collaborative(rec2, file_path)

#file_path <- "IBCF_jacard.csv"
#rec_list <- collaborative(rec3, file_path)

#file_path <- "POPULAR.csv"
#rec_list <- collaborative(rec4, file_path)
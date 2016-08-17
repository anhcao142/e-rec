init_full_data <- function(filename, no_rating) {
  data = read.table("ratings.dat", sep=":", fill=FALSE, strip.white=TRUE)
  d <- data[sample(nrow(data), no_rating), ]
  d$V2 = NULL
  d$V4 = NULL
  d$V6 = NULL
  d <- d[order(d$V7), ]
  d$V7 <- NULL
  write.table(d,file="ratings.txt",row.names=FALSE,col.names=FALSE,sep='\t')
}
train_test <- function(filename, no_test_ratings) {
  d <- read.table(filename,header=FALSE, sep = "\t")
  names(d) <- c("user", "item", "rating")
  l = length(d$user)-no_test_ratings
  
  test <- d
  test$rating[1:l] <- 0
  
  train <- d
  train$rating[(l+1):length(d$user)] <- 0
  
  write.table(test,file="test.txt",row.names=FALSE,col.names=FALSE,sep='\t')
  write.table(train,file="train.txt",row.names=FALSE,col.names=FALSE,sep='\t')
}


update_file <- function(filename, index, no_update_rating, no_init_rating, maxindex) {
  print(index);
  #read train
  d <- read.table(filename,header=FALSE, sep = "\t")
  
  # create update file 
  update <- d
  i1 = no_init_rating+ no_update_rating*index
  i2 = no_init_rating+ no_update_rating*(index+1)+1
  
  update$V3[1:i1] <- 0
  
  if(index != maxindex) {
    update$V3[i2:length(d$V2)] <- 0
  }
  name <- paste(index,"-u",".txt",sep="")
  write.table(update,file=name,row.names=FALSE,col.names=FALSE,sep='\t')
  
  # create train file
  train <- d
  train$V3[i2: length(d$V2)] <- 0
  
  name <- paste(index,"-t",".txt",sep="")
  write.table(train,file=name,row.names=FALSE,col.names=FALSE,sep='\t')
  
  #create related file
  relate <- d[0:i1,]
  
  t1= i1+1
  t2 = i2-1
  a = d[t1:t2,]
  relate <- cbind(relate, 0)
  for(i in 1: nrow(a)) {
    if(nrow(relate[relate$V1 == a$V1[i],]) > 0) {
      relate[relate$V1 == a$V1[i],]$'0' <- 1
    }
    if(nrow(relate[relate$V2 == a$V2[i],]) > 0) {
      relate[relate$V2 == a$V2[i],]$'0' <- 1
    }
  }
  
  relate[relate$`0`==0,]$V3<- 0
  relate$`0` = NULL
  
  d$V3 <- 0
  relate <- rbind(relate, d[t1:length(d$V1),])
  name <- paste(index,"-r",".txt",sep="")
  write.table(relate,file=name,row.names=FALSE,col.names=FALSE,sep='\t')
  print("done");
}

exp1 <- function() {
  filename = "ratings.dat"
  no_rating = 650000
  init_full_data(filename, no_rating)
  
  no_test_ratings = 100000
  filename = "ratings.txt"
  train_test(filename, no_test_ratings)
  
  filename = "train.txt"
  no_update_rating = 4000
  no_init_rating = 500000
  maxindex = 9
  
  d <- read.table(filename,header=FALSE, sep = "\t")
  i1 <- no_init_rating +1
  d$V3[i1: length(d$V2)] <- 0
  write.table(d,file="init.txt",row.names=FALSE,col.names=FALSE,sep='\t')
  
  for(index in 0:maxindex ) {
    update_file(filename, index, no_update_rating, no_init_rating, maxindex)
  }
}
exp1()
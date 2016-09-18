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


update_file <- function(filename, index, no_update_object, no_init_rating, maxindex) {
  print(index);
  #read train
  d <- read.table(filename,header=FALSE, sep = "\t")
  
  
  
  # create update file 
  i1 = no_init_rating
  init <- d[1:i1,]
  
  
  update <- d[(i1+1):length(d$V3),]
  item <- NULL
  user <- NULL
  k= 0
  for(i in 1: length(update$V3)) {
    if(k <500) {
      if((length(user) + length(item))<no_update_object) {
        if(!update$V1[i] %in% user)
          user <- rbind(user, update$V1[i])
        if(!update$V2[i] %in% item)
          item <- rbind(item, update$V2[i])
      }
      else {
        if((!update$V1[i] %in% user) || (!update$V2[i] %in% item))
          update$V3[i] <- 0
      }
      k = k+1
    }
    else {
      update$V3[i] <- 0
    }
  }
  u <- init
  u$V3 <- 0
  u <- rbind(u, update)
  name <- paste(index,"-u",".txt",sep="")
  write.table(u,file=name,row.names=FALSE,col.names=FALSE,sep='\t')
  
  # create train file
  train <- rbind(init, update)
  
  name <- paste(index,"-t",".txt",sep="")
  write.table(train,file=name,row.names=FALSE,col.names=FALSE,sep='\t')
  
  #create related file
  relate <- d[0:i1,]
  t1= i1+1
  a <- update
  relate <- cbind(relate, 0)
  for(i in 1: length(user)){
    if(nrow(relate[relate$V1 == user[i],]) > 0) {
      relate[relate$V1 == user[i],]$'0' <- 1
    }
  }
  for(i in 1: length(item)) {
    if(nrow(relate[relate$V2 == item[i],]) > 0) {
      relate[relate$V2 == item[i],]$'0' <- 1
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
  no_rating = 200000
  init_full_data(filename, no_rating)
  
  no_test_ratings = 20000
  filename = "ratings.txt"
  train_test(filename, no_test_ratings)
  
  filename = "train.txt"
  no_init_rating = 100000
  maxindex = 1
  
  d <- read.table(filename,header=FALSE, sep = "\t")
  i1 <- no_init_rating +1
  d$V3[i1: length(d$V2)] <- 0
  write.table(d,file="init.txt",row.names=FALSE,col.names=FALSE,sep='\t')
  
  no_update_object = 500
  
  for(index in 0:0) {
    update_file(filename, index, no_update_object, no_init_rating, maxindex)
  }
  
  no_update_object = 400
  
  for(index in 1:1 ) {
    update_file(filename, index, no_update_object, no_init_rating, 1)
  }
  no_update_object = 300
  
  for(index in 2:2 ) {
    update_file(filename, index, no_update_object, no_init_rating, maxindex)
  }
  no_update_object = 200
  
  for(index in 3:3 ) {
    update_file(filename, index, no_update_object, no_init_rating, maxindex)
  }
  no_update_object = 100
  
  for(index in 4:4) {
    update_file(filename, index, no_update_object, no_init_rating, maxindex)
  }
  no_update_object = 50
  
  for(index in 5:5) {
    update_file(filename, index, no_update_object, no_init_rating, maxindex)
  }
}
exp1()
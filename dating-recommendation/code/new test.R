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
train_test <- function(filename, percent) {
  d <- read.table(filename,header=FALSE, sep = "\t")
  names(d) <- c("user", "item", "rating")
  l = length(d$user)*percent
  
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

update_file_change_init <- function(filename, index, no_update_rating, no_init_rating, maxindex, f_index) {
  print(f_index);
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
  name <- paste(f_index,"-u",".txt",sep="")
  write.table(update,file=name,row.names=FALSE,col.names=FALSE,sep='\t')
  
  # create train file
  train <- d
  train$V3[i2: length(d$V2)] <- 0
  
  name <- paste(f_index,"-t",".txt",sep="")
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
  name <- paste(f_index,"-r",".txt",sep="")
  write.table(relate,file=name,row.names=FALSE,col.names=FALSE,sep='\t')
  print("done");
}


train_test_fix_number<-function(filename, no_test) {
  d <- read.table(filename,header=FALSE, sep = "\t")
  names(d) <- c("user", "item", "rating")
  l = length(d$user)-no_test
  
  test <- d
  test$rating[1:l] <- 0
  
  train <- d
  train$rating[(l+1):length(d$user)] <- 0
  
  write.table(test,file="test.txt",row.names=FALSE,col.names=FALSE,sep='\t')
  write.table(train,file="train.txt",row.names=FALSE,col.names=FALSE,sep='\t')
}

exp6 <- function() {
  filename = "ratings.dat"
  no_rating = 1000000
  init_full_data(filename, no_rating)
  
  filename = "ratings.txt"
  train_test(filename, percent)
  
  filename = "train.txt"
  no_update_rating = 10000
  no_init_rating = 800000
  maxindex = 8
  
  d <- read.table(filename,header=FALSE, sep = "\t")
  i1 <- no_init_rating +1
  d$V3[i1: length(d$V2)] <- 0
  write.table(d,file="init.txt",row.names=FALSE,col.names=FALSE,sep='\t')
  
  for(index in 0:8) {
    update_file(filename, index, no_update_rating, no_init_rating, maxindex)
  }
}

exp7 <- function(no_rating, no_init_rating, no_update_rating, maxindex) {
  filename = "ratings.dat"
  init_full_data(filename, no_rating)
  
  filename = "ratings.txt"
  train_test_fix_number(filename, 10000)
  
  filename = "train.txt"
  
  d <- read.table(filename,header=FALSE, sep = "\t")
  i1 <- no_init_rating +1
  d$V3[i1: length(d$V2)] <- 0
  write.table(d,file="init.txt",row.names=FALSE,col.names=FALSE,sep='\t')
  
  
  for(index in 0:maxindex) {
    no_update = no_update_rating*(index+1)
    update_file_change_init(filename, 0,no_update, no_init_rating, maxindex, index)
  }
}

#exp7(210000, 100000, 10000, 9)
#exp7(66000, 50000, 200, 29)
exp7(116000, 100000, 200, 29)



init_full_data <- function(filename, no_rating) {
  data = read.table("ratings.dat", sep=":", fill=FALSE, strip.white=TRUE, nrows = 1000000)
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
##generate data for experiment 1
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

#index: 0:9

exp1 <- function() {
filename = "ratings.dat"
no_rating = 100000
init_full_data(filename, no_rating)

percent = 0.9
filename = "ratings.txt"
train_test(filename, percent)

filename = "train.txt"
no_update_rating = 2000
no_init_rating = 20000
maxindex = 34

d <- read.table(filename,header=FALSE, sep = "\t")
i1 <- no_init_rating +1
d$V3[i1: length(d$V2)] <- 0
write.table(d,file="init.txt",row.names=FALSE,col.names=FALSE,sep='\t')

for(index in 0:34) {
  update_file(filename, index, no_update_rating, no_init_rating, maxindex)
}
}

##generate data for experiment 2
update_file2 <- function(filename, index) {
  print(index);
  #read train
  d <- read.table(filename,header=FALSE, sep = "\t")
  
  name <- paste(index,"-t",".txt",sep="")
  t <- read.table(name,header=FALSE, sep = "\t")

  
  #create related file
  relate <- t
  
  a = d[d$V3!=0,]
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
  
  name <- paste(index,"-r",".txt",sep="")
  write.table(relate,file=name,row.names=FALSE,col.names=FALSE,sep='\t')
  print("done");
}


##generate data for experiment 3
update_file3 <- function(filename, index, no_update_rating, no_init_rating, maxindex, file_name) {
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
  name <- paste(file_name,"-u",".txt",sep="")
  write.table(update,file=name,row.names=FALSE,col.names=FALSE,sep='\t')
  
  # create train file
  train <- d
  train$V3[i2: length(d$V2)] <- 0
  
  name <- paste(file_name,"-t",".txt",sep="")
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
  name <- paste(file_name,"-r",".txt",sep="")
  write.table(relate,file=name,row.names=FALSE,col.names=FALSE,sep='\t')
  print("done");
}

exp3 <- function() {
  filename = "train.txt"
  no_update_ratings = c(1, 100, 1000, 5000, 10000, 20000)
  no_init_ratings = 70000
  maxindex = 34
  
  d <- read.table(filename,header=FALSE, sep = "\t")
  i1 <- no_init_ratings +1
  d$V3[i1: length(d$V2)] <- 0
  write.table(d,file="init.txt",row.names=FALSE,col.names=FALSE,sep='\t')
  
  for(index in 0:5) {
    n <- no_update_ratings[index+1]
    update_file2(filename, 0, n, no_init_ratings, maxindex, index)
  }
  
}

exp2 <- function() {
  for(index in 0:6) {
    filename = "u.txt"
    update_file2(filename, index)
  }
}



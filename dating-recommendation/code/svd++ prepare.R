
d1 = read.table("SVD++-rating-predictions fold [1].txt", sep=" ", fill=FALSE, strip.white=TRUE)
d2 = read.table("SVD++-rating-predictions fold [2].txt", sep=" ", fill=FALSE, strip.white=TRUE)
d3 = read.table("SVD++-rating-predictions fold [3].txt", sep=" ", fill=FALSE, strip.white=TRUE)
d4 = read.table("SVD++-rating-predictions fold [4].txt", sep=" ", fill=FALSE, strip.white=TRUE)
d5 = read.table("SVD++-rating-predictions fold [5].txt", sep=" ", fill=FALSE, strip.white=TRUE)
d1$V4 = NULL
d2$V4 = NULL
d3$V4 = NULL
d4$V4 = NULL
d5$V4 = NULL
l1 = 25
l2 = 10
  
  tr <- rbind(d1, d2, d3, d4)
  names(tr) <- c("user", "item", "rating")
  test <- d5
  names(test) <- c("user", "item", "rating")
  
  g<-acast(tr, user ~ item)
  uID <- row.names(g)
  average <- mean(g, na.rm=TRUE)
  
  file_path <- "5.csv"
  rec_list <- bellkov_prediction(file_path)
  
  
  tr <- rbind(d1, d2, d3, d5)
  names(tr) <- c("user", "item", "rating")
  test <- d4
  names(test) <- c("user", "item", "rating")
  
  g<-acast(tr, user ~ item)
  uID <- row.names(g)
  average <- mean(g, na.rm=TRUE)
  
  file_path <- "4.csv"
  rec_list <- bellkov_prediction(file_path)
  
  
  tr <- rbind(d1, d2, d5, d4)
  names(tr) <- c("user", "item", "rating")
  test <- d3
  names(test) <- c("user", "item", "rating")
  
  g<-acast(tr, user ~ item)
  uID <- row.names(g)
  average <- mean(g, na.rm=TRUE)
  
  file_path <- "3.csv"
  rec_list <- bellkov_prediction(file_path)
  
  
  tr <- rbind(d1, d5, d3, d4)
  names(tr) <- c("user", "item", "rating")
  test <- d2
  names(test) <- c("user", "item", "rating")
  
  g<-acast(tr, user ~ item)
  uID <- row.names(g)
  average <- mean(g, na.rm=TRUE)
  
  file_path <- "2.csv"
  rec_list <- bellkov_prediction(file_path)
  
  tr <- rbind(d5, d2, d3, d4)
  names(tr) <- c("user", "item", "rating")
  test <- d1
  names(test) <- c("user", "item", "rating")
  
  
  g<-acast(tr, user ~ item)
  uID <- row.names(g)
  average <- mean(g, na.rm=TRUE)
  
  file_path <- "1.csv"
  rec_list <- bellkov_prediction(file_path)
  
t1 <- read.csv('1.csv', header = F)
t2 <- read.csv('2.csv', header = F)
t3 <- read.csv('3.csv', header = F)
t4 <- read.csv('4.csv', header = F)
t5 <- read.csv('5.csv', header = F)
tt1 = read.table("SVD++-rating-predictions fold [1].txt", sep=" ", fill=FALSE, strip.white=TRUE)

test6 <- rbind(t1, t2, t3, t4, t5)
r6 <- run(t5)
  
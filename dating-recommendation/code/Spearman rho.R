d1 = read.table("SVD++-rating-predictions fold [1].txt", sep=" ", fill=FALSE, strip.white=TRUE)
d2 = read.table("SVD++-rating-predictions fold [2].txt", sep=" ", fill=FALSE, strip.white=TRUE)
d3 = read.table("SVD++-rating-predictions fold [3].txt", sep=" ", fill=FALSE, strip.white=TRUE)
d4 = read.table("SVD++-rating-predictions fold [4].txt", sep=" ", fill=FALSE, strip.white=TRUE)
d5 = read.table("SVD++-rating-predictions fold [5].txt", sep=" ", fill=FALSE, strip.white=TRUE)

t1 <- read.csv('1.csv', header = F)
t2 <- read.csv('2.csv', header = F)
t3 <- read.csv('3.csv', header = F)
t4 <- read.csv('4.csv', header = F)
t5 <- read.csv('5.csv', header = F)

n = nrow(d)
d <- rbind(d1, d2, d3, d4, d5)
e1 <- 1-6*sum(d$V3-d$V4)*sum(d$V3-d$V4)/(n*(n*n -1))

t <- rbind(t1, t2, t3, t4, t5)
e2 <- 1-6*sum(t$V3-t$V4)*sum(t$V3-t$V4)/(n*(n*n -1))

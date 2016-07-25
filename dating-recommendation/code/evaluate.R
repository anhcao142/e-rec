for(index in 0:6) {
  name <- paste(index,"-r",".txt",sep="")
  d <- read.table(name,header=FALSE, sep = "\t")
  s <- d[d$V3!=0,]
  print(index);
  print(nrow(s))
}

for(index in 0:6) {
  name <- paste(index,"-u",".txt",sep="")
  d <- read.table(name,header=FALSE, sep = "\t")
  s <- d[d$V3!=0,]
  s1 <- s[!duplicated(s$V1), ]$V1
  s2 <- s[!duplicated(s$V2), ]$V2
  n <- length(s1) + length(s2)
  print(index);
  print(n)
}
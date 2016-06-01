d = read.table("20 000.txt", sep="\t", fill=FALSE, strip.white=TRUE)
d <- d[order(d$V4, decreasing = TRUE),]

d$V4 = NULL
d_2 = d[2001:2200,]
d_3 = d[2201:2400,]
d_4 = d[2401:2600,]
d_5 = d[2601:2800,]
d_6 = d[2801:3000,]

dd = read.table("20 000_0.txt", sep="\t", fill=FALSE, strip.white=TRUE)

dd1 = dd
a = d_6
dd1 <- cbind(dd1, 0)
for(i in 1: nrow(a)) {
  print(i);
  dd1[dd1$V1 == a$V1[i],]$'0' <- 1
  dd1[dd1$V2 == a$V2[i],]$'0' <- 1
}
dd1[dd1$`0`==0,]$V3 <- 0
dd1$`0` = NULL
write.table(dd1,file="20 000_5t.txt",row.names=FALSE,col.names=FALSE,sep='\t')

dd1 = dd
a = d_5
dd1 <- cbind(dd1, 0)
for(i in 1: nrow(a)) {
  print(i);
  dd1[dd1$V1 == a$V1[i],]$'0' <- 1
  dd1[dd1$V2 == a$V2[i],]$'0' <- 1
}
dd1[dd1$`0`==0,]$V3 <- 0
dd1$`0` = NULL
write.table(dd1,file="20 000_4t.txt",row.names=FALSE,col.names=FALSE,sep='\t')

dd1 = dd
a = d_4
dd1 <- cbind(dd1, 0)
for(i in 1: nrow(a)) {
  print(i);
  dd1[dd1$V1 == a$V1[i],]$'0' <- 1
  dd1[dd1$V2 == a$V2[i],]$'0' <- 1
}
dd1[dd1$`0`==0,]$V3 <- 0
dd1$`0` = NULL
write.table(dd1,file="20 000_3t.txt",row.names=FALSE,col.names=FALSE,sep='\t')

dd1 = dd
a = d_4
dd1 <- cbind(dd1, 0)
for(i in 1: nrow(a)) {
  print(i);
  dd1[dd1$V1 == a$V1[i],]$'0' <- 1
  dd1[dd1$V2 == a$V2[i],]$'0' <- 1
}
dd1[dd1$`0`==0,]$V3 <- 0
dd1$`0` = NULL
write.table(dd1,file="20 000_2t.txt",row.names=FALSE,col.names=FALSE,sep='\t')

dd1 = dd
a = d_3
dd1 <- cbind(dd1, 0)
for(i in 1: nrow(a)) {
  print(i);
  dd1[dd1$V1 == a$V1[i],]$'0' <- 1
  dd1[dd1$V2 == a$V2[i],]$'0' <- 1
}
dd1[dd1$`0`==0,]$V3 <- 0
dd1$`0` = NULL
write.table(dd1,file="20 000_2t.txt",row.names=FALSE,col.names=FALSE,sep='\t')

dd1 = dd
a = d_2
dd1 <- cbind(dd1, 0)
for(i in 1: nrow(a)) {
  print(i);
  dd1[dd1$V1 == a$V1[i],]$'0' <- 1
  dd1[dd1$V2 == a$V2[i],]$'0' <- 1
}
dd1[dd1$`0`==0,]$V3 <- 0
dd1$`0` = NULL
write.table(dd1,file="20 000_1t.txt",row.names=FALSE,col.names=FALSE,sep='\t')
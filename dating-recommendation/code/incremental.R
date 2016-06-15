data = read.table("50_0.txt", sep="\t", fill=FALSE, strip.white=TRUE)
set.seed(7)
ss <- sample(1:6,size=nrow(data),replace=TRUE,prob=c(0.9,0.004,0.004,0.004,0.004,0.004 ))
train <- data[ss==1,]
t1 <- data[ss==2,]
t2 <- data[ss==3,]
t3 <- data[ss==4,]
t4 <- data[ss==5,]
t5 <- data[ss==6,]

write.table(train,file="50_0.txt",row.names=FALSE,col.names=FALSE,sep='\t')
write.table(t1,file="50_1.txt",row.names=FALSE,col.names=FALSE,sep='\t')
write.table(t2,file="50_2.txt",row.names=FALSE,col.names=FALSE,sep='\t')
write.table(t3,file="50_3.txt",row.names=FALSE,col.names=FALSE,sep='\t')
write.table(t4,file="50_4.txt",row.names=FALSE,col.names=FALSE,sep='\t')
write.table(t5,file="50_5.txt",row.names=FALSE,col.names=FALSE,sep='\t')


data = read.table("ratings.dat", sep=":", fill=FALSE, strip.white=TRUE, nrows = 1000000)
data$V2 = NULL
data$V4 = NULL
data$V6 = NULL
set.seed(7)
ss <- sample(1:2,size=nrow(data),replace=TRUE,prob=c(0.9,0.1))
d <- data[ss==2,]
write.table(d,file="100 000.txt",row.names=FALSE,col.names=FALSE,sep='\t')

d = read.table("20 000.txt", sep="\t", fill=FALSE, strip.white=TRUE)
d <- d[order(d$V7, decreasing = TRUE),]
d1 <- head(d,1000)
write.table(d1,file="20 000_test.txt",row.names=FALSE,col.names=FALSE,sep='\t')
d2 = d[1001:1100,]
d3 = d[1101:1200,]
d4 = d[1201:1300,]
d5 = d[1301:1400,]
d6 = d[1401:1500,]
d0 = d[1501: length(d$V1),]
d7 = d[1001:length(d$V1),]
write.table(d0,file="20 000_0.txt",row.names=FALSE,col.names=FALSE,sep='\t')
write.table(d2,file="20 000_1.txt",row.names=FALSE,col.names=FALSE,sep='\t')
write.table(d3,file="20 000_2.txt",row.names=FALSE,col.names=FALSE,sep='\t')
write.table(d4,file="20 000_3.txt",row.names=FALSE,col.names=FALSE,sep='\t')
write.table(d5,file="20 000_4.txt",row.names=FALSE,col.names=FALSE,sep='\t')
write.table(d6,file="20 000_5.txt",row.names=FALSE,col.names=FALSE,sep='\t')
write.table(d7,file="20 000_train.txt",row.names=FALSE,col.names=FALSE,sep='\t')


d7 = read.table("20 000_train.txt", sep="\t", fill=FALSE, strip.white=TRUE)
d1 = read.table("20 000_test.txt", sep="\t", fill=FALSE, strip.white=TRUE)
d0 = read.table("20 000_0.txt", sep="\t", fill=FALSE, strip.white=TRUE)
d2 = read.table("20 000_1.txt", sep="\t", fill=FALSE, strip.white=TRUE)
d3 = read.table("20 000_2.txt", sep="\t", fill=FALSE, strip.white=TRUE)
d4 = read.table("20 000_3.txt", sep="\t", fill=FALSE, strip.white=TRUE)
d5 = read.table("20 000_4.txt", sep="\t", fill=FALSE, strip.white=TRUE)
d6 = read.table("20 000_5.txt", sep="\t", fill=FALSE, strip.white=TRUE)

d0$V4 = NULL
d1$V4 = NULL
d2$V4 = NULL
d3$V4 = NULL
d4$V4 = NULL
d5$V4 = NULL
d6$V4 = NULL
d7$V4 = NULL

d = read.table("20 000.txt", sep="\t", fill=FALSE, strip.white=TRUE)
d <- d[order(d$V4, decreasing = TRUE),]

d$V4 = NULL
d_1 <- head(d,1000)
d_2 = d[2001:2200,]
d_3 = d[2201:2400,]
d_4 = d[2401:2600,]
d_5 = d[2601:2800,]
d_6 = d[2801:3000,]
d_0 = d[3001: length(d$V1),]

d$V3[d$V3 >0] <- 0
d1 <- rbind(d_1, d[2001:length(d$V1),])
d2 <- rbind(d[0:2000,],d_2 , d[2201:length(d$V1),])
d3 <- rbind(d[0:2200,],d_3 , d[2401:length(d$V1),])
d4 <- rbind(d[0:2400,],d_4 , d[2601:length(d$V1),])
d5 <- rbind(d[0:2600,],d_5 , d[2801:length(d$V1),])
d6 <- rbind(d[0:2800,],d_6 , d[3001:length(d$V1),])
d0 <- rbind(d[0:3000,],d_0)

write.table(d0,file="20 000_0.txt",row.names=FALSE,col.names=FALSE,sep='\t')
write.table(d2,file="20 000_1.txt",row.names=FALSE,col.names=FALSE,sep='\t')
write.table(d3,file="20 000_2.txt",row.names=FALSE,col.names=FALSE,sep='\t')
write.table(d4,file="20 000_3.txt",row.names=FALSE,col.names=FALSE,sep='\t')
write.table(d5,file="20 000_4.txt",row.names=FALSE,col.names=FALSE,sep='\t')
write.table(d6,file="20 000_5.txt",row.names=FALSE,col.names=FALSE,sep='\t')
#write.table(d1,file="20 000_test.txt",row.names=FALSE,col.names=FALSE,sep='\t')

d6 <- rbind(d[0:2800,],d_6 , d_0)
d5 <- rbind(d[0:2600,],d_5 , d6[2801:length(d$V1),])
d4 <- rbind(d[0:2400,],d_4 , d5[2601:length(d$V1),])
d3 <- rbind(d[0:2200,],d_3 , d4[2401:length(d$V1),])
d2 <- rbind(d[0:2000,],d_2 , d3[2201:length(d$V1),])

#write.table(d0,file="20 000_0n.txt",row.names=FALSE,col.names=FALSE,sep='\t')
write.table(d2,file="20 000_1n.txt",row.names=FALSE,col.names=FALSE,sep='\t')
write.table(d3,file="20 000_2n.txt",row.names=FALSE,col.names=FALSE,sep='\t')
write.table(d4,file="20 000_3n.txt",row.names=FALSE,col.names=FALSE,sep='\t')
write.table(d5,file="20 000_4n.txt",row.names=FALSE,col.names=FALSE,sep='\t')
write.table(d6,file="20 000_5n.txt",row.names=FALSE,col.names=FALSE,sep='\t')

d = read.table("null 20 000.txt", sep="\t", fill=FALSE, strip.white=TRUE)
d <- d[order(d$V4, decreasing = TRUE),]

d$V4 = NULL
d_1 <- head(d,1000)
d_2 = d[1001:1500,]
d_3 = d[1501:2000,]
d_4 = d[2001:2500,]
d_5 = d[2501:3000,]
d_6 = d[3001:3500,]
d_0 = d[3501: length(d$V1),]

d$V3[d$V3 >0] <- 0
d1 <- rbind(d_1, d[1001:length(d$V1),])
d2 <- rbind(d[0:1000,],d_2 , d[1501:length(d$V1),])
d3 <- rbind(d[0:1500,],d_3 , d[2001:length(d$V1),])
d4 <- rbind(d[0:2000,],d_4 , d[2501:length(d$V1),])
d5 <- rbind(d[0:2500,],d_5 , d[3001:length(d$V1),])
d6 <- rbind(d[0:3000,],d_6 , d[3501:length(d$V1),])
d0 <- rbind(d[0:3500,],d_0)

write.table(d0,file="20 029_0.txt",row.names=FALSE,col.names=FALSE,sep='\t')
write.table(d2,file="20 029_1.txt",row.names=FALSE,col.names=FALSE,sep='\t')
write.table(d3,file="20 029_2.txt",row.names=FALSE,col.names=FALSE,sep='\t')
write.table(d4,file="20 029_3.txt",row.names=FALSE,col.names=FALSE,sep='\t')
write.table(d5,file="20 029_4.txt",row.names=FALSE,col.names=FALSE,sep='\t')
write.table(d6,file="20 029_5.txt",row.names=FALSE,col.names=FALSE,sep='\t')
write.table(d1,file="20 029_test.txt",row.names=FALSE,col.names=FALSE,sep='\t')

d6 <- rbind(d[0:3000,],d_6 , d_0)
d5 <- rbind(d[0:2500,],d_5 , d6[3001:length(d$V1),])
d4 <- rbind(d[0:2000,],d_4 , d5[2501:length(d$V1),])
d3 <- rbind(d[0:1500,],d_3 , d4[2001:length(d$V1),])
d2 <- rbind(d[0:1000,],d_2 , d3[1501:length(d$V1),])

#write.table(d0,file="20 000_0n.txt",row.names=FALSE,col.names=FALSE,sep='\t')
write.table(d2,file="20 029_1n.txt",row.names=FALSE,col.names=FALSE,sep='\t')
write.table(d3,file="20 029_2n.txt",row.names=FALSE,col.names=FALSE,sep='\t')
write.table(d4,file="20 029_3n.txt",row.names=FALSE,col.names=FALSE,sep='\t')
write.table(d5,file="20 029_4n.txt",row.names=FALSE,col.names=FALSE,sep='\t')
write.table(d6,file="20 029_5n.txt",row.names=FALSE,col.names=FALSE,sep='\t')



d = read.table("100 000.txt", sep="\t", fill=FALSE, strip.white=TRUE)
d <- d[order(d$V4, decreasing = TRUE),]

d$V4 = NULL
d_1 <- head(d,5000)
d_2 = d[5001:7000,]
d_3 = d[7001:9000,]
d_4 = d[9001:11000,]
d_5 = d[11001:13000,]
d_6 = d[13001:15000,]
d_0 = d[15001: length(d$V1),]

d$V3[d$V3 >0] <- 0
d1 <- rbind(d_1, d[5001:length(d$V1),])
d2 <- rbind(d[0:5000,],d_2 , d[7001:length(d$V1),])
d3 <- rbind(d[0:7000,],d_3 , d[9001:length(d$V1),])
d4 <- rbind(d[0:9000,],d_4 , d[11001:length(d$V1),])
d5 <- rbind(d[0:11000,],d_5 , d[13001:length(d$V1),])
d6 <- rbind(d[0:13000,],d_6 , d[15001:length(d$V1),])
d0 <- rbind(d[0:15000,],d_0)

#write.table(d0,file="100 000_0.txt",row.names=FALSE,col.names=FALSE,sep='\t')
write.table(d2,file="100 000_1.txt",row.names=FALSE,col.names=FALSE,sep='\t')
write.table(d3,file="100 000_2.txt",row.names=FALSE,col.names=FALSE,sep='\t')
write.table(d4,file="100 000_3.txt",row.names=FALSE,col.names=FALSE,sep='\t')
write.table(d5,file="100 000_4.txt",row.names=FALSE,col.names=FALSE,sep='\t')
write.table(d6,file="100 000_5.txt",row.names=FALSE,col.names=FALSE,sep='\t')
#write.table(d1,file="100 000_test.txt",row.names=FALSE,col.names=FALSE,sep='\t')

d6 <- rbind(d[0:13000,],d_6 , d_0)
d5 <- rbind(d[0:11000,],d_5 , d6[13001:length(d$V1),])
d4 <- rbind(d[0:9000,],d_4 , d5[11001:length(d$V1),])
d3 <- rbind(d[0:7000,],d_3 , d4[9001:length(d$V1),])
d2 <- rbind(d[0:5000,],d_2 , d3[7001:length(d$V1),])

#write.table(d0,file="20 000_0n.txt",row.names=FALSE,col.names=FALSE,sep='\t')
write.table(d2,file="100 000_1n.txt",row.names=FALSE,col.names=FALSE,sep='\t')
write.table(d3,file="100 000_2n.txt",row.names=FALSE,col.names=FALSE,sep='\t')
write.table(d4,file="100 000_3n.txt",row.names=FALSE,col.names=FALSE,sep='\t')
write.table(d5,file="100 000_4n.txt",row.names=FALSE,col.names=FALSE,sep='\t')
write.table(d6,file="100 000_5n.txt",row.names=FALSE,col.names=FALSE,sep='\t')


d$V4 = NULL
d_1 <- d[5001,]
d$V3[d$V3 >0] <- 0
d1 <- rbind(d[0:5000,],d_1 , d[5002:length(d$V1),])
write.table(d1,file="100 000_1_1.txt",row.names=FALSE,col.names=FALSE,sep='\t')

d = read.table("100 000_0.txt", sep="\t", fill=FALSE, strip.white=TRUE)

dd1 = d
a = d_1
dd1 <- cbind(dd1, 0)
for(i in 1: nrow(a)) {
  print(i);
  dd1[dd1$V1 == a$V1[i],]$'0' <- 1
  dd1[dd1$V2 == a$V2[i],]$'0' <- 1
}
dd1[dd1$`0`==0,]$V3 <- 0
dd1$`0` = NULL
write.table(dd1,file="100 000_1_1t.txt",row.names=FALSE,col.names=FALSE,sep='\t')

d = read.table("100 000_test.txt", sep="\t", fill=FALSE, strip.white=TRUE)
dd1 <- cbind(d, 0)
dd1[dd1$V1 == 3125 & dd1$V2 == 707,]$'0' <- 1
dd1[dd1$`0`==0,]$V3 <- 0
dd1$`0` = NULL
write.table(dd1,file="100 000_1_test.txt",row.names=FALSE,col.names=FALSE,sep='\t')

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


data = read.table("ratings.dat", sep=":", fill=FALSE, strip.white=TRUE, nrows = 100000)
data$V2 = NULL
data$V4 = NULL
data$V6 = NULL
set.seed(7)
ss <- sample(1:2,size=nrow(data),replace=TRUE,prob=c(0.8,0.2))
d <- data[ss==2,]
write.table(d,file="20 000.txt",row.names=FALSE,col.names=FALSE,sep='\t')

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
d_2 = d[1001:1050,]
d_3 = d[1051:1100,]
d_4 = d[1101:1150,]
d_5 = d[1151:1200,]
d_6 = d[1201:1250,]
d_0 = d[1251: length(d$V1),]

d$V3[d$V3 >0] <- 0
d1 <- rbind(d_1, d[1001:length(d$V1),])
d2 <- rbind(d[0:1000,],d_2 , d[1051:length(d$V1),])
d3 <- rbind(d[0:1050,],d_3 , d[1101:length(d$V1),])
d4 <- rbind(d[0:1100,],d_4 , d[1151:length(d$V1),])
d5 <- rbind(d[0:1150,],d_5 , d[1201:length(d$V1),])
d6 <- rbind(d[0:1200,],d_6 , d[1251:length(d$V1),])
d0 <- rbind(d[0:1250,],d_0)

write.table(d0,file="20 000_0.txt",row.names=FALSE,col.names=FALSE,sep='\t')
write.table(d2,file="20 000_1.txt",row.names=FALSE,col.names=FALSE,sep='\t')
write.table(d3,file="20 000_2.txt",row.names=FALSE,col.names=FALSE,sep='\t')
write.table(d4,file="20 000_3.txt",row.names=FALSE,col.names=FALSE,sep='\t')
write.table(d5,file="20 000_4.txt",row.names=FALSE,col.names=FALSE,sep='\t')
write.table(d6,file="20 000_5.txt",row.names=FALSE,col.names=FALSE,sep='\t')
#write.table(d1,file="20 000_test.txt",row.names=FALSE,col.names=FALSE,sep='\t')

d6 <- rbind(d[0:1250,],d_6 , d_0)
d5 <- rbind(d[0:1200,],d_5 , d6[1251:length(d$V1),])
d4 <- rbind(d[0:1150,],d_4 , d5[1201:length(d$V1),])
d3 <- rbind(d[0:1100,],d_3 , d4[1151:length(d$V1),])
d2 <- rbind(d[0:1050,],d_2 , d3[1101:length(d$V1),])

#write.table(d0,file="20 000_0n.txt",row.names=FALSE,col.names=FALSE,sep='\t')
write.table(d2,file="20 000_1n.txt",row.names=FALSE,col.names=FALSE,sep='\t')
write.table(d3,file="20 000_2n.txt",row.names=FALSE,col.names=FALSE,sep='\t')
write.table(d4,file="20 000_3n.txt",row.names=FALSE,col.names=FALSE,sep='\t')
write.table(d5,file="20 000_4n.txt",row.names=FALSE,col.names=FALSE,sep='\t')
write.table(d6,file="20 000_5n.txt",row.names=FALSE,col.names=FALSE,sep='\t')

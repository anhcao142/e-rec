d = read.table("20 000.txt", sep="\t", fill=FALSE, strip.white=TRUE)
d <- d[order(d$V4, decreasing = TRUE),]

d$V4 = NULL
d_2 = d[1001:1500,]
d_3 = d[1501:2000,]
d_4 = d[2001:2500,]
d_5 = d[2501:3000,]
d_6 = d[3001:3500,]
d_7 = d[3501:4000,]
d_8 = d[4001:4500,]
d_9 = d[4501:5000,]
d_10 = d[5001:5500,]
d_11 = d[5501:6000,]
d_0 = d[6001: length(d$V1),]

d$V3[d$V3 >0] <- 0
d2 <- rbind(d[0:1000,],d_2 , d[1501:length(d$V1),])
d3 <- rbind(d[0:1500,],d_3 , d[2401:length(d$V1),])
d4 <- rbind(d[0:2000,],d_4 , d[2601:length(d$V1),])
d5 <- rbind(d[0:2500,],d_5 , d[2801:length(d$V1),])
d6 <- rbind(d[0:3000,],d_6 , d[3501:length(d$V1),])
d7 <- rbind(d[0:3500,],d_7 , d[4001:length(d$V1),])
d8 <- rbind(d[0:4000,],d_8 , d[4501:length(d$V1),])
d9 <- rbind(d[0:4500,],d_9 , d[5001:length(d$V1),])
d10 <- rbind(d[0:5000,],d_10 , d[5501:length(d$V1),])
d11 <- rbind(d[0:5500,],d_11 , d[6001:length(d$V1),])
d0 <- rbind(d[0:6000,],d_0)

write.table(d0,file="20 000_0.txt",row.names=FALSE,col.names=FALSE,sep='\t')
write.table(d2,file="20 000_1.txt",row.names=FALSE,col.names=FALSE,sep='\t')
write.table(d3,file="20 000_2.txt",row.names=FALSE,col.names=FALSE,sep='\t')
write.table(d4,file="20 000_3.txt",row.names=FALSE,col.names=FALSE,sep='\t')
write.table(d5,file="20 000_4.txt",row.names=FALSE,col.names=FALSE,sep='\t')
write.table(d6,file="20 000_5.txt",row.names=FALSE,col.names=FALSE,sep='\t')
write.table(d7,file="20 000_6.txt",row.names=FALSE,col.names=FALSE,sep='\t')
write.table(d8,file="20 000_7.txt",row.names=FALSE,col.names=FALSE,sep='\t')
write.table(d9,file="20 000_8.txt",row.names=FALSE,col.names=FALSE,sep='\t')
write.table(d10,file="20 000_9.txt",row.names=FALSE,col.names=FALSE,sep='\t')
write.table(d11,file="20 000_10.txt",row.names=FALSE,col.names=FALSE,sep='\t')

d11 <- rbind(d[0:5500,],d_11 , d_0)
d10 <- rbind(d[0:5000,],d_10 , d11[5501:length(d$V1),])
d9 <- rbind(d[0:4500,],d_9 , d10[5001:length(d$V1),])
d8 <- rbind(d[0:4000,],d_8 , d9[4501:length(d$V1),])
d7 <- rbind(d[0:3500,],d_7 , d8[4001:length(d$V1),])
d6 <- rbind(d[0:3000,],d_6 , d7[3501:length(d$V1),])
d5 <- rbind(d[0:2500,],d_5 , d6[3001:length(d$V1),])
d4 <- rbind(d[0:2000,],d_4 , d5[2501:length(d$V1),])
d3 <- rbind(d[0:1500,],d_3 , d4[2001:length(d$V1),])
d2 <- rbind(d[0:1000,],d_2 , d3[1501:length(d$V1),])


write.table(d2,file="20 000_1n.txt",row.names=FALSE,col.names=FALSE,sep='\t')
write.table(d3,file="20 000_2n.txt",row.names=FALSE,col.names=FALSE,sep='\t')
write.table(d4,file="20 000_3n.txt",row.names=FALSE,col.names=FALSE,sep='\t')
write.table(d5,file="20 000_4n.txt",row.names=FALSE,col.names=FALSE,sep='\t')
write.table(d6,file="20 000_5n.txt",row.names=FALSE,col.names=FALSE,sep='\t')
write.table(d7,file="20 000_6n.txt",row.names=FALSE,col.names=FALSE,sep='\t')
write.table(d8,file="20 000_7n.txt",row.names=FALSE,col.names=FALSE,sep='\t')
write.table(d9,file="20 000_8n.txt",row.names=FALSE,col.names=FALSE,sep='\t')
write.table(d10,file="20 000_9n.txt",row.names=FALSE,col.names=FALSE,sep='\t')
write.table(d11,file="20 000_10n.txt",row.names=FALSE,col.names=FALSE,sep='\t')

dd = read.table("20 000_0.txt", sep="\t", fill=FALSE, strip.white=TRUE)


create <- function (){
  dd1 = dd
  dd1 <- cbind(dd1, 0)
  for(i in 1: nrow(a)) {
    dd1[dd1$V1 == a$V1[i],]$'0' <- 1
    dd1[dd1$V2 == a$V2[i],]$'0' <- 1
  }
  dd1[dd1$`0`==0,]$V3 <- 0
  dd1$`0` = NULL
  write.table(dd1,file=filename,row.names=FALSE,col.names=FALSE,sep='\t')
}

a = d_2
filename = "20 000_1t.txt"
create()

dd = read.table("100 000_0.txt", sep="\t", fill=FALSE, strip.white=TRUE)

a = d_3
filename = "100 000_2t.txt"
create()

a = d_2
filename = "100 000_1t.txt"
create()
item_freq <- read.csv('item.csv', header = TRUE)
data <- read.table("ratings.dat",header=FALSE, sep = ",")
names(data) <- c("user", "item", "rating")

for(i in 1: 20) {
  item <- item_freq[item_freq$Freq<=i*20,]
  item <- item[order(item$Freq, decreasing = TRUE),]
  item <- item[1:100,]
  toBeKeeped <- which(data$item %in% item$item )
  tr <- data[toBeKeeped,]
  file_name <- paste(i*5, '.csv', sep = "")
  write.table(tr,file=file_name,row.names=FALSE,col.names=TRUE,sep=',')
}



user_freq <- read.csv('user.csv', header = TRUE)
data <- read.table("ratings.dat",header=FALSE, sep = ",")
names(data) <- c("user", "item", "rating")
data <- data[order(data$item),]

for(i in 1: 20) {
  user <- user_freq[user_freq$Freq<=i*20,]
  user <- user[order(user$Freq, decreasing = TRUE),]
  user <- user[1:100,]
  toBeKeeped <- which(data$user %in% user$user )
  tr <- data[toBeKeeped,]
  file_name <- paste(i*5, '.csv', sep = "")
  write.table(tr,file=file_name,row.names=FALSE,col.names=TRUE,sep=',')
}


rating_density <- function () {
  for(i in 1: 20) {
    file_name <- paste(i*5, '.csv', sep = "")
    d <- read.csv(file_name,header = TRUE)
    user <- length(d$user[!duplicated(d$user)])
    item <- length(d$item[!duplicated(d$item)])
    density <- length(d$rating)/(user*item)
    write.table(density,"MAE.csv", row.names=F,na="NA",append=T, quote= FALSE, sep=",", col.names=F)
    write.table(density,"RMSE.csv", row.names=F,na="NA",append=T, quote= FALSE, sep=",", col.names=T)
    print(density)
    flush.console()
    print(i)
    flush.console()
  }
}
rating_density()



create <- function() {
  user_freq <- read.csv('user.csv', header = TRUE)
  data <- read.table("ratings.dat",header=FALSE, sep = ",")
  names(data) <- c("user", "item", "rating")
  user <- user_freq[order(user_freq$Freq, decreasing = TRUE),]
  user <- user[1:1500,]
  toBeKeeped <- which(data$user %in% user$user )
  data <- data[toBeKeeped,]
  item <- data$item
  count <- as.data.frame(table(item))
  count <- count[order(count$Freq, decreasing = TRUE),]
  count$item <- as.numeric(levels(count$item))[count$item]
  item <- count[1:100,]
  toBeKeeped <- which(data$item %in% item$item )
  data <- data[toBeKeeped,]
  write.table(data,file='best_data.csv',row.names=FALSE,col.names=TRUE,sep=',')
  data <- read.csv('best_data.csv', header = TRUE)
  item <- data$item
  count <- as.data.frame(table(item))
  count <- count[order(count$Freq, decreasing = TRUE),]
  count$item <- as.numeric(levels(count$item))[count$item]
  write.table(count,file='item.csv',row.names=FALSE,col.names=TRUE,sep=',')
  user <- data$user
  count <- as.data.frame(table(user))
  count <- count[order(count$Freq, decreasing = TRUE),]
  count$user <- as.numeric(levels(count$user))[count$user]
  write.table(count,file='user.csv',row.names=FALSE,col.names=TRUE,sep=',')
}

#item_freq <- read.csv('item.csv', header = TRUE)
data <- read.csv("best_data.csv",header=TRUE)
#names(data) <- c("user", "item", "rating")
num = 2
d <- split(data, data$item)
dd <- split(data, data$user)
for(i in 1: 20) {
  tr <- NULL
  for(j in 1: length(d)) {
    a <- d[[j]][sample(nrow(d[[j]]), num*i), ]
    tr <- rbind(a, tr)
  }
  user <- tr$user[!duplicated(tr$user)]
  print(i)
  flush.console()
  for(k in 1: length(user)) {
    b <- dd[[as.character(user[k])]]
    if(nrow(b)>(num*i)) {
      b <- b[sample(nrow(b), num*i), ]
    }
    tr <- rbind(b, tr)
  }
  tr <- tr[!duplicated(tr),]
  file_name <- paste(i*5, '.csv', sep = "")
  write.table(tr,file=file_name,row.names=FALSE,col.names=TRUE,sep=',')
}

data <- read.csv("best_data.csv",header=TRUE)
user_freq <- read.csv('user.csv', header = TRUE)
#names(data) <- c("user", "item", "rating")
num = 5
#d <- split(data, data$item)
dd <- split(data, data$user)
for(i in 1: 20) {
  tr <- NULL
  for(j in 1: 100) {
    a <- dd[[as.character(user_freq$user[j])]]
    if(nrow(a)>(num*i)) {
      a <- a[sample(nrow(a), num*i), ]
    }
    tr <- rbind(a, tr)
  }
  print(i)
  flush.console()
  file_name <- paste(i*5, '.csv', sep = "")
  write.table(tr,file=file_name,row.names=FALSE,col.names=TRUE,sep=',')
}


data <- read.csv("best_data.csv",header=TRUE)
item_freq <- read.csv('item.csv', header = TRUE)
#names(data) <- c("user", "item", "rating")
num = 5
d <- split(data, data$item)
#dd <- split(data, data$user)
for(i in 1: 20) {
  tr <- NULL
  for(j in 1: 100) {
    a <- d[[as.character(item_freq$item[j])]]
    if(nrow(a)>(num*i)) {
      a <- a[sample(nrow(a), num*i), ]
    }
    tr <- rbind(a, tr)
  }
  print(i)
  flush.console()
  file_name <- paste(i*5, '.csv', sep = "")
  write.table(tr,file=file_name,row.names=FALSE,col.names=TRUE,sep=',')
}

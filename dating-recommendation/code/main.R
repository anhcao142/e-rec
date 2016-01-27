setwd("~/e-rec/dating-recommendation/test_number_item/14-20")

tr <- read.csv('train.csv', header = TRUE)
test<-read.csv("test.csv",header=TRUE)
g<-acast(tr, user ~ item)
uID <- row.names(g)
average <- mean(g, na.rm=TRUE)

file_path <- "BASELINE.csv"
rec_list <- baseline_prediction(file_path)

file_path <- "BELLKOV.csv"
rec_list <- bellkov_prediction(file_path)

test5 <- read.csv("BASELINE.csv", header = TRUE)
test6 <- read.csv("BELLKOV.csv", header = TRUE)
run(test5)
run(test6)
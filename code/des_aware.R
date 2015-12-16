library(tm)
library(SnowballC)

create_event_top_words <- function(file_path) {
  options( stringsAsFactors=F )
  events_description <- read.csv(file_path, as.is=T)
  events_description <- events_description[order(events_description$eventId),]
  events_description <- events_description[!duplicated(events_description$eventId),]
  description_list <- as.list(events_description[,"description"])
  names(description_list) <- events_description$eventId
  
  my.docs <- VectorSource(description_list)
  my.corpus <- Corpus(my.docs)
  names(my.corpus) <- events_description$eventId
  
  my.corpus <- tm_map(my.corpus, removePunctuation)
  my.corpus <- tm_map(my.corpus, stemDocument)
  my.corpus <- tm_map(my.corpus, removeNumbers)
  my.corpus <- tm_map(my.corpus, content_transformer(tolower))
  my.corpus <- tm_map(my.corpus, stripWhitespace)
  
  dtm <- DocumentTermMatrix(my.corpus, control = list(weighting = weightTfIdf))
  dtm_matrix <- as.matrix(dtm)
  events_top_word <- matrix(data=NA, nrow = nrow(dtm_matrix), ncol = 100)
  rownames(events_top_word) <- rownames(dtm_matrix)
  for(i in 1: nrow(dtm_matrix)) {
    words <- sort(dtm_matrix[i,], decreasing = TRUE)
    top_word <- matrix(data = NA, nrow = 1, ncol = 100)
    top_word <- as.vector(top_word)
    for(j in 1: min(100, length(words))) {
      if(words[[j]] == 0) 
        break
      top_word[j] <- paste(names(words)[j],"-",words[[j]],sep="")
      print(top_word[j])
      flush.console()
    }
    events_top_word[i,] <- top_word
    print(i)
    flush.console()
  }
  return (events_top_word)
}

create_user_top_words <- function(history, events_top_word) {
  lst <- list()
  for(i in 1: length(history)) {
    lst[[i]] <- events_top_word[[as.character(history[i])]]
  }
  user_tw <- tapply(unlist(lst), names(unlist(lst)), sum)
  user_tw <- sort(user_tw, decreasing = TRUE)
  
  top_word <- matrix(data = NA, nrow = 1, ncol = 100)
  top_word <- as.vector(top_word)
  if(length(user_tw) == 0)
    return (top_word)
  for(j in 1: min(100, length(user_tw))) {
    if(user_tw[[j]] == 0) 
      break
    top_word[j] <- paste(names(user_tw)[j],"-",user_tw[[j]],sep="")
  }
  return (top_word)
}

create_user_top_word <- function() {
 
  file_path <- 'Archive 3/16-12/events_top_words.csv'
  events <- read.csv(file_path, header = T, row.names = 1)
  events_top_word <- vector(mode="list", length=nrow(events))
  names(events_top_word) <- row.names(events)
  
  for(i in 1: nrow(events)) {
    event_tw <- events[i, ]
    print(i)
    flush.console()
    event_tw <- parse_top_word(event_tw)
    events_top_word[[i]] <- event_tw
  }
  
  file_path <- "Archive 3/13-12/prepare.csv"
  user_history <- get_users_history(file_path)
  
  users_top_word <- matrix(data=NA, nrow = length(user_history), ncol = 100)
  rownames(users_top_word) <- names(user_history)
    
  for(i in 1 : length(user_history)) {
    if(!length(user_history[[i]])==0) {
      user_tw <- create_user_top_words(user_history[[i]], events_top_word)
      users_top_word[i, ] <- user_tw
    }
  }
  write.csv(users_top_word, 'Archive 3/16-12/users_top_words.csv', row.names=T)
}

write_events_top_words <- function() {
  file_path <- 'Archive 3/event_des.csv'
  events_top_word <- create_event_top_words(file_path)
  write.csv(events_top_word, 'Archive 3/16-12/events_top_words.csv', row.names=T)
}

parse_top_word_file <-function(df) {
  tw_object<- vector(mode="list", length=nrow(df))
  names(tw_object) <- row.names(df)
  for(i in 1: nrow(df)) {
    tw <- df[i, ]
    print(i)
    flush.console()
    tw <- parse_top_word(tw)
    tw_object[[i]] <- tw
  }
  
  return(tw_object)
}


parse_top_word <- function(event_tw) {
  event_tw <- event_tw[!is.na(event_tw)]
  event_top_word <- vector()
  if(!length(event_tw) == 0) {
    rnames <- vector()
    for(j in 1 : length(event_tw)) {
      temp <- strsplit(event_tw[j], "-")
      rnames <- c(rnames, temp[[1]][1])
      event_top_word[j] <-  as.numeric(temp[[1]][2])
    }
    names(event_top_word) <- rnames
  }
  return (event_top_word)
}

# 
calculate_word_score_for_one_user <- function(user_tw, event_tw) {
  if(length(user_tw) == 0)
    return (0)
  tmp <- matrix(data = 0, nrow = 1, ncol = length(user_tw))
  tmp <- as.vector(tmp)
  names(tmp) <- names(user_tw)
  is_null <- T
  for(i in 1: length(user_tw)) {
    index <- as.character(names(user_tw)[i])
    if(!is.na(event_tw[index])) {
      tmp[i] <- event_tw[index]
      is_null <- F
    }
  }
  if(is_null==T)
    return (0)
  return(cosine(user_tw, tmp)[1, 1])
  
}
create_user_word_score <- function(file_path, des_path) {
  data.set <- read.csv(file_path, header = T)
  users_top_word <- read.csv("Archive 3/16-12/users_top_words.csv", header = T, row.names = 1)
  event_top_word <- read.csv("Archive 3/16-12/events_top_words.csv", header = T, row.names = 1)
  
  users_top_word <- parse_top_word_file(users_top_word)
  event_top_word <- parse_top_word_file(event_top_word)
  
  word_score <- vector(mode="numeric", length = nrow(data.set))
  for(i in 1: nrow(data.set)) {
    userID <- data.set$userID[i]
    eventID <- data.set$eventID[i]
    user_tw <- users_top_word[[as.character(userID)]]
    event_tw <- event_top_word[[as.character(eventID)]]
    word_score[i] <- calculate_word_score_for_one_user(user_tw, event_tw)
  }
  
  data.set <- cbind(data.set, word_score)
  write.csv(data.set, des_path, row.names=F)
}

create_scored_files <- function() {
  train_scored <- read.csv("Archive 3/13-12/train_score.csv", header = T)
  test_scored <- read.csv("Archive 3/13-12/test_score.csv", header = T)
  train_word_score <- read.csv("Archive 3/16-12/train_word_score.csv", header = T)
  test_word_score <- read.csv("Archive 3/16-12/test_word_score.csv", header = T)
  train_scored <- cbind(train_scored, word_score=train_word_score$word_score)
  write.csv(train_scored, "Archive 3/16-12/train_score.csv", row.names=F)
  test_scored <- cbind(test_scored, word_score=test_word_score$word_score)
  write.csv(test_scored, "Archive 3/16-12/test_score.csv", row.names=F)
}

create_word_score_train_test <- function() {
  file_path <- "Archive 3/13-12/train.csv"
  des_path <- "Archive 3/16-12/train_word_score.csv"
  create_user_word_score(file_path, des_path)
  
  file_path <- "Archive 3/13-12/test.csv"
  des_path <- "Archive 3/16-12/test_word_score.csv"
  create_user_word_score(file_path, des_path)
}

#write_events_top_words()
#create_user_top_word()
#create_word_score_train_test()
create_scored_files

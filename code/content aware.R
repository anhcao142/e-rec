library(lsa)
library(GenKern)

get_users_history <- function(file_path) {
  attendee <- read.csv(file_path)
  attendee <- attendee[order(attendee$userID),]
  userID <- attendee$userID
  userID <- userID[!duplicated(userID)]
  user_history <- vector(mode="list", length=length(userID))
  names(user_history) <- userID
  j <- 1
  history <- vector()
  
  for(i in 1:nrow(attendee)){
    if(attendee$userID[i] == userID[j]) {
      if(attendee$status[i] == 1) {
        history <- c(history, attendee$eventID[i])
      }
    } else {
      user_history[[j]] <- history
      j <- j+1
      history <- vector()
      if(attendee$status[i] == 1) {
        history <- c(history, attendee$eventID[i])
      }
    }
  }
  user_history[[j]] <- history
  return(user_history)
}


## create event time vector for one event
create_event_time_vector <- function(start_time, during_time) {
  matrix <- matrix(data=0,nrow=24,ncol=7)
  time_vector <- as.vector(matrix)
  if(is.na(start_time) || is.na(during_time)) {
    return(time_vector)
  }
  index <- start_time
  for(i in 1: during_time) {
    if(index > 168)
      index <- 1
    time_vector[index] <- 1
    index <- index+1
  }
  return(time_vector)
}

calculate_time_score_for_one_user <- function(user_time_vector, event_time_vector) {
  a <- as.vector(user_time_vector)
  b <- as.vector(event_time_vector)
  return(cosine(a, b)[1, 1])
}

calculate_location_score_for_one_user <- function(user_long, user_lat, event_long, event_lat) {
  if(is.na(event_long) || is.na(event_lat))
    return (0)
  x <- as.vector(user_long)
  y <- as.vector(user_lat)
  location_score <- KernSur(x,y, xgridsize=1, ygridsize=1, correlation=0, 
                xbandwidth=1, ybandwidth=1, range.x=c(event_long, event_long), range.y=c(event_lat, event_lat))
  return (location_score$zden[1, 1])
}

#df : longitude list or latitude list
read_location_element <- function (df) {
  location_element <- vector(mode="list", length=nrow(df))
  names(location_element) <- rownames(df)
  for(i in 1: nrow(df)) {
    print(i)
    flush.console()
    lst <- as.character(df[i, 1])
    d <- strsplit(lst, "-")
    t <- as.numeric(d[[1]])
    location_element[[i]] <- t
  }
  return(location_element)
}

calculate_time_score <- function(file_path, des_path) {
  data.set <- read.csv(file_path, header = T)
  users_time_vector <- read.csv("29-12/users_time_vector.csv", header = T, row.names = 1)
  events_data <- read.csv("events_data.csv", header = T, row.names = 1)
  time_score <- vector(mode="numeric", length = nrow(data.set))
  
  for(i in 1: nrow(data.set)) {
    userID <- data.set$userID[i]
    eventID <- data.set$eventID[i]
    event_time_vector <- create_event_time_vector(events_data[as.character(eventID),]$start_time, events_data[as.character(eventID),]$during_time)
    user_time_vector <- unname(unlist(users_time_vector[as.character(userID),]))
    t_score <- calculate_time_score_for_one_user(user_time_vector, event_time_vector)
    if(!is.na(t_score))
      time_score[i] <- t_score
  }
  data.set <- cbind(data.set, time_score)
  write.csv(data.set, des_path, row.names=F)
}

calculate_location_score <- function(file_path, des_path) {
  data.set <- read.csv(file_path, header = T)
  users_longitude <- read.csv("29-12/users_longitude.csv", header = T, row.names = 1)
  users_latitude <- read.csv("29-12/users_latitude.csv", header = T, row.names = 1)
  users_longitude <- read_location_element(users_longitude)
  users_latitude <- read_location_element(users_latitude)
  events_data <- read.csv("events_data.csv", header = T, row.names = 1)
  location_score <- vector(mode="numeric", length = nrow(data.set))
  for(i in 1: nrow(data.set)) {
    print(i)
    flush.console()
    userID <- data.set$userID[i]
    eventID <- data.set$eventID[i]
    
    user_long <- users_longitude[[as.character(userID)]]
    user_long <- user_long[!is.na(user_long)]
    user_lat <- users_latitude[[as.character(userID)]]
    user_lat <- user_lat[!is.na(user_lat)]
    if(length(user_long)==0 || length(user_lat)==0) {
      location_score[i] <- 0
    } else {
      location_score[i] <- calculate_location_score_for_one_user(user_long, user_lat, events_data[as.character(eventID),]$longitude, events_data[as.character(eventID),]$latitude)
    }
  }
  data.set <- cbind(data.set, location_score)
  write.csv(data.set, des_path, row.names=F)
}

file_path <- "29-12/train.csv"
des_path <- "29-12/train_time_score.csv"
calculate_time_score(file_path, des_path)
file_path <- "29-12/test.csv"
des_path <- "29-12/test_time_score.csv"
calculate_time_score(file_path, des_path)
file_path <- "29-12/train.csv"
des_path <- "29-12/train_location_score.csv"
calculate_location_score(file_path, des_path)
file_path <- "29-12/test.csv"
des_path <- "29-12/test_location_score.csv"
calculate_location_score(file_path, des_path)


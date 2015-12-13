library(lsa)
library(GenKern)

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

calculate_score <- function(file_path) {
  data.set <- read.csv(file_path, header = T)
  users_time_vector <- read.csv("Archive 3/13-12/users_time_vector.csv", header = T, row.names = 1)
  users_longitude <- read.csv("Archive 3/13-12/users_longitude.csv", header = T, row.names = 1)
  users_latitude <- read.csv("Archive 3/13-12/users_latitude.csv", header = T, row.names = 1)
  events_data <- read.csv("Archive 3/events_data.csv", header = T, row.names = 1)
  time_score <- vector(mode="numeric", length = nrow(data.set))
  location_score <- vector(mode="numeric", length = nrow(data.set))
  
  for(i in 1: nrow(data.set)) {
    userID <- data.set$userID[i]
    eventID <- data.set$eventID[i]
    event_time_vector <- create_event_time_vector(events_data[as.character(eventID),]$start_time, events_data[as.character(eventID),]$during_time)
    user_time_vector <- unname(unlist(users_time_vector[as.character(userID),]))
    t_score <- calculate_time_score_for_one_user(user_time_vector, event_time_vector)
    if(!is.na(t_score))
      time_score[i] <- t_score
    
    user_long <- unname(unlist(users_longitude[as.character(userID),]))
    user_long <- user_long[!is.na(user_long)]
    user_lat <- unname(unlist(users_latitude[as.character(userID),]))
    user_lat <- user_lat[!is.na(user_lat)]
    if(length(user_long)==0 || length(user_lat)==0) {
      location_score[i] <- 0
    } else {
      location_score[i] <- calculate_location_score_for_one_user(user_long, user_lat, events_data[as.character(eventID),]$longitude, events_data[as.character(eventID),]$latitude)
    }
  }
  data.set <- cbind(data.set, time_score)
  data.set <- cbind(data.set, location_score)
  write.csv(data.set, 'Archive 3/13-12/train_score.csv', row.names=T)
  #write.csv(data.set, 'Archive 3/13-12/test_score.csv', row.names=T)
}

file_path <- "Archive 3/13-12/train.csv"
calculate_score(file_path)
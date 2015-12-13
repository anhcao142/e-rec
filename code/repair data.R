library(lubridate)
require(devtools)
source_gist(4676064)

prepare_attendee_file <- function(file_path) {
  user_event <- read.csv(file_path)
  attendee <- matrix(data=NA, nrow = nrow(user_event), ncol = 3)
  attendee <- data.frame(attendee)
  colnames(attendee)  <- c("userID","eventID", "status")
  for (i in 1: nrow(user_event)) {
    if(!is.na(user_event$attending[i])) {
      attendee$userID[i] <- user_event$attending[i]
      attendee$status[i] <- 1
    }
    else {
      if(!is.na(user_event$declined[i])) {
        attendee$userID[i] <- user_event$declined[i]
        attendee$status[i] <- -1
      } else {
        attendee$userID[i] <- user_event$maybe[i]
        attendee$status[i] <- 0
        print(i)
        flush.console()
      }
    }
    attendee$eventID[i] <- user_event$eventId[i]
  }
  write.csv(attendee, 'Archive 3/attendee_with_status.csv', row.names=F)
}

remove_inefficient_data <- function(file_path) {
  attendee <- read.csv(file_path)
  attendee <- attendee[order(attendee$userID),]
  attendee_no_inefficient <- attendee
  count <- 1
  number_of_removed <- 0

  for (i in 1: (nrow(attendee)-1)) { 
    if(attendee$userID[i] == attendee$userID[i+1]) {
      count <- count + 1
    } else {
      if(count == 1) {
        attendee_no_inefficient <- attendee_no_inefficient[-c(i-number_of_removed),]
        number_of_removed <- number_of_removed + 1
        print(i)
        flush.console()
      }
      count <- 1
    }
  }
  if(count == 1)
    attendee_no_inefficient <- attendee_no_inefficient[-c(nrow(attendee-number_of_removed)),]
  
  write.csv(attendee_no_inefficient, 'Archive 3/attendee_no_inefficient.csv', row.names=F)
}

remove_bias_data <- function(file_path) {
  attendee <- read.csv(file_path)
  attendee <- attendee[order(attendee$userID),]
  attendee_no_inefficient <- attendee
  count <- 1
  attendee_count <- 1
  number_of_removed <- 0
  for (i in 1: (nrow(attendee)-1)) { 
    if(attendee$userID[i] == attendee$userID[i+1]) {
      if(attendee$status[i]==1) 
        attendee_count <- attendee_count + 1
      count <- count + 1
    } else {
      if(count == 1) {
        attendee_no_inefficient <- attendee_no_inefficient[-c(i-number_of_removed),]
        number_of_removed <- number_of_removed + 1
        print(i)
        flush.console()
      } else {
        if(attendee_count > 100) {
          print(attendee_count)
          flush.console()
          print(count)
          flush.console()
          for(j in 1: count) {
            print(attendee_no_inefficient$userID[i-number_of_removed])
            flush.console()
            attendee_no_inefficient <- attendee_no_inefficient[-c(i-number_of_removed),]
            number_of_removed <- number_of_removed + 1
          }
        }
      }
      attendee_count <- 1
      count <- 1
    }
  }
  if(count == 1) {
    attendee_no_inefficient <- attendee_no_inefficient[-c(nrow(attendee-number_of_removed)),]
  } else {
    if(attendee_count > 100) {
      for(j in 1: count) {
        attendee_no_inefficient <- attendee_no_inefficient[-c(i-number_of_removed),]
        number_of_removed <- number_of_removed + 1
      }
    }
  }
  print(number_of_removed)
  flush.console()
  write.csv(attendee_no_inefficient, 'Archive 3/attendee_no_inefficient_no_bias.csv', row.names=F)
  
}

prepare_event <- function(file_path) {
  events <- read.csv(file_path)
  events <- events[order(events$eventId),]
  events <- events[!duplicated(events$eventId),]
  events_data <- matrix(data = NA, nrow = nrow(events), ncol = 4)
  events_data <- data.frame(events_data)
  rownames(events_data) <- events$eventId
  colnames(events_data) <- c("start_time", "during_time", "latitude", "longitude")
  
  for (i in 1: nrow(events)) {
    if(!is.na(events$latitude[i]) && !is.na(events$longitude[i])) {
      events_data$latitude[i] <- events$latitude[i]
      events_data$longitude[i] <- events$longitude[i]
    }
    if(!(is.na(events$startTime[i]) | events$startTime[i]=="") && !(is.na(events$endTime[i]) | events$endTime[i]=="")) {
      start <- as.POSIXct(events$startTime[i], format = "%Y-%m-%dT%H:%M:%S%z");
      start.day <- wday(as.Date(events$startTime[i],'%Y-%m-%d'))
      start.time <- as.POSIXlt(events$startTime[i], format = "%Y-%m-%dT%H:%M:%S%z");
      
      end <- as.POSIXct(events$endTime[i], format = "%Y-%m-%dT%H:%M:%S%z");
      
      diff <- difftime(end, start, units = "hour")
      events_data$during_time[i] <- min(168, diff)
      events_data$start_time[i] <- (start.day - 1) * 24 + start.time$hour
    }
  }
  write.csv(events_data, 'Archive 3/events_data.csv', row.names=T)
}

create_train_test <- function(file_path) {
  data <- read.csv(file_path)
  set.seed(7)
  ss <- sample(1:3,size=nrow(data),replace=TRUE,prob=c(0.7,0.25,0.05))
  prepare <- data[ss==1,]
  train <- data[ss==2,]
  test <- data[ss==3,]
  write.csv(prepare, 'Archive 3/13-12/prepare.csv', row.names=F)
  write.csv(train, 'Archive 3/13-12/train.csv', row.names=F)
  write.csv(test, 'Archive 3/13-12/test_result.csv', row.names=F)
}

prepare_data <- function() {
  file_path <- "Archive 3/event-user.csv"
  prepare_attendee_file(file_path)
  
  #file_path <- "Archive 3/attendee_with_status.csv"
  #remove_inefficient_data(file_path)
  
  file_path <- "Archive 3/attendee_with_status.csv"
  remove_bias_data(file_path)
  
  file_path <- "Archive 3/events.csv"
  prepare_event(file_path)
  
  file_path <- "Archive 3/attendee_no_inefficient_no_bias.csv"
  create_train_test(file_path)
}

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

create_user_time_data <- function(history, events_data) {
  matrix <- matrix(data=0,nrow=24,ncol=7)
  time_vector <- as.vector(matrix)
  if(length(history) == 0) {
    return (time_vector)
  }
  for(i in 1: length(history)) {
    start_time <- events_data[as.character(history[i]),]$start_time
    during_time <- events_data[as.character(history[i]),]$during_time
    if(!is.na(start_time) && !is.na(during_time)) {
      time_vector <- time_vector + create_event_time_vector(start_time, during_time)
    }
  }
  time_vector <- time_vector / length(history)
  return (time_vector)
}

create_user_location_data <- function(history, events_data) {
  longitude <- vector()
  latitude <- vector()
  if(length(history) == 0) {
    return (list(longitude = longitude, latitude = latitude))
  }
  for(i in 1: length(history)) {
    lat <- events_data[as.character(history[i]),]$latitude
    long <- events_data[as.character(history[i]),]$longitude
    if(!is.na(lat) && !is.na(long)) {
      longitude <- c(longitude, long)
      latitude <- c(latitude, lat)
    }
  }
  return (list(longitude = longitude, latitude = latitude))
}


create_user_data <- function() {
  events_data <- read.csv("Archive 3/events_data.csv", header = T, row.names = 1)
  
  file_path <- "Archive 3/13-12/prepare.csv"
  user_history <- get_users_history(file_path)
  
  user_time <- matrix(data = NA, nrow = length(user_history), ncol = 168)
  user_time <- data.frame(user_time)
  rownames(user_time) <- names(user_history)
  colnames(user_time) <- c(1:168)
  
  user_longitude <- vector(mode="list", length=length(user_history))
  user_latitude <- vector(mode="list", length=length(user_history))
  names(user_longitude) <- names(user_history)
  names(user_latitude) <- names(user_history)
  
  for(i in 1 : length(user_history)) {
    user_time[i,] <- create_user_time_data(user_history[[i]], events_data)
    user_location <- create_user_location_data(user_history[[i]], events_data)
    user_longitude[[i]] <- user_location$longitude
    user_latitude[[i]] <- user_location$latitude
  }
  
  user_latitude <- as.data.frame(user_latitude)
  user_longitude <- as.data.frame(user_longitude)
  
  write.csv(user_time, 'Archive 3/13-12/users_time_vector.csv', row.names=T)
  write.csv(user_longitude, 'Archive 3/13-12/users_longitude.csv', row.names=T)
  write.csv(user_latitude, 'Archive 3/13-12/users_latitude.csv', row.names=T)
   
}

create_test_file <- function() {
  test <- read.csv('Archive 3/13-12/test_result.csv')
  test$status <- NULL
  write.csv(test, 'Archive 3/13-12/test.csv', row.names=F)
}

#prepare_data()
#create_test_file()
create_user_data()

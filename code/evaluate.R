rmse <- function(obs, pred) {
    e <- (as.numeric(obs)-as.numeric(pred))^2
    m <- mean(e)
    
    return (sqrt(m))
}

obs <- read.csv('./16-12/test_result.csv')
pred <- read.csv('./16-12/test_predict_2.csv')

t <- pred$status
lower <- -0.5
upper <- 0.5

print(paste0('Lower: ', lower))
print(paste0('Upper: ', upper))

for (i in 1:length(pred$status)) {
    if (pred$status[i] < lower) {
        t[i] <- -1
    } else if (pred$status[i] > upper) {
        t[i] <- 1
    } else {
        t[i] <- 0
    }
}

result <- rmse(obs$status, pred$status)
print(paste0('RMSE: ', result))

combine <- data.frame(obs = obs$status, pre = t)
goRatio <- nrow(combine[which(combine$obs == 1 & combine$pre == 1), ])/nrow(combine[which(combine$obs == 1), ])
maybeRatio <- nrow(combine[which(combine$obs == 0 & combine$pre == 0), ])/nrow(combine[which(combine$obs == 0), ])
declinedRatio <- nrow(combine[which(combine$obs == -1 & combine$pre == -1), ])/nrow(combine[which(combine$obs == -1), ])

print(paste0('Go recall: ', goRatio))
print(paste0('Maybe recall: ', maybeRatio))
print(paste0('Declined recall: ', declinedRatio))

goRatioRev <- nrow(combine[which(combine$obs == 1 & combine$pre == 1), ])/nrow(combine[which(combine$pre == 1), ])
maybeRatioRev <- nrow(combine[which(combine$obs == 0 & combine$pre == 0), ])/nrow(combine[which(combine$pre == 0), ])
declinedRatioRev <- nrow(combine[which(combine$obs == -1 & combine$pre == -1), ])/nrow(combine[which(combine$pre == -1), ])

print(paste0('Go precision: ', goRatioRev))
print(paste0('Maybe precision: ', maybeRatioRev))
print(paste0('Declined precision: ', declinedRatioRev))
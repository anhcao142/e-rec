evaluate <- function (predict_path, result_path) {
  predict <- read.csv(predict_path, header = T)
  result <- read.csv(result_path, header = T)
  tp = 0
  fp = 0
  tn = 0
  fn = 0
  for(i in 1: nrow(predict)) {
    if(predict$status[i]>0.5) {
      predict$status[i] == 1
      if(result$status[i]==1) {
        tp = tp +1
      } else {
        fp = fp +1
      }
    } else {
      predict$status[i] == 0
      if(result$status[i]==1) {
        fn = fn +1
      } else {
        tn = tn +1
      }
    }
  }
  
  precision <- tp/(tp+fp)
  recall <- tp /(tp+fn)
  accuracy <- (tp+tn)/(tp+tn+fp+fn)
  specificity <- tn/(tn+fp)
  f1 <- 2*(precision*recall)/(precision+recall)
  
  print(paste('tp:', as.character(tp), ',','fn:', as.character(fn), ',','fp:', as.character(fp), ',','tn:', as.character(tn)))
  print(paste('precision',precision))
  print(paste('recall',recall))
  print(paste('accuracy',accuracy))
  print(paste('specificity', specificity))
  print(paste('f1', f1))
}


result_path <- "29-12/test_result.csv"
predict_path <- "29-12/test_word_predict.csv"
evaluate(predict_path, result_path)

result_path <- "bigger 7/test_result.csv"
predict_path <- "bigger 7/test_word_predict.csv"
evaluate(predict_path, result_path)
predict_path <- "bigger 7/test_time_predict.csv"
evaluate(predict_path, result_path)
predict_path <- "bigger 7/test_location_predict.csv"
evaluate(predict_path, result_path)

result_path <- "30-12/test_result.csv"
predict_path <- "30-12/test_word_predict.csv"
evaluate(predict_path, result_path)
predict_path <- "30-12/test_time_predict.csv"
evaluate(predict_path, result_path)
predict_path <- "30-12/test_location_predict.csv"
evaluate(predict_path, result_path)


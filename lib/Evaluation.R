### ADS Project 4 Spring 2018
### Fangbing Liu
### Evaluation - Ranked Score (for MS data)
###            - MAE (for Movie data)
###            - ROC (for Movie data)

## Ranked Score (the larger, the better)

RS <- function(predict, test, d = 0, alpha = 5){
  ## Input: predict - predicted test set
  ##        test - true test set
  ##        d - netural vote
  ##        alpha - viewing halflife
  rank_pred <- ncol(predict)+1-t(apply(predict, 1, function(mat){return(rank(mat, ties.method = "first"))}))
  rank_test <- ncol(test)+1-t(apply(test, 1, function(mat){return(rank(mat, ties.method = "first"))}))
  vec <- ifelse(test - d > 0, test - d, 0)
  R <- apply(vec/2^((rank_pred-1)/(alpha-1)), 1, sum)
  R_max <- apply(vec/2^((rank_test-1)/(alpha-1)), 1, sum)
  
  RS <- 100*sum(R)/sum(R_max)
  return(RS)
}


## MAE (the smaller, the better)

MAE <- function(predict, test){
  MAE <- mean(abs(predict - test), na.rm = T)
  return(MAE)
}

## ROC (the larger, the better)

ROC_4 <- function(predict, test){
  ## set filter = 4, which means the rating > 4 is good item, rating <=4 is bad item
  ## actual ratings are from 0-5, correspond to the data 1-6
  filter <- matrix(4, nrow = nrow(predict), ncol = ncol(predict))
  good_num <- sum((predict > filter) == (test > filter), na.rm = T)
  n <- sum(!is.na(predict))
  
  return(good_num/n)
}



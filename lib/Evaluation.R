### ADS Project 4 Spring 2018
### Fangbing Liu, Yanjun Lin
### Evaluation - Ranked Score (for MS data)
###            - MAE (for Movie data)
###            - ROC (for Movie data)

## Ranked Score (the larger, the better)

rank_score <- function(pred_set, true_set){
  # here we set d = 0 since for MS data we only have positive sample
  items <- ncol(true_set)
  users <- nrow(true_set)
  
  rank_pred <- items+1-t(apply(pred_set,1,rank,ties.method="first"))
  rank_ture <-  items+1-t(apply(true_set,1,rank,ties.method="first"))
  
  weight_pred <- t(apply(rank_pred,1,function(x){return(1/2^((x-1)/(5-1)))}))
  weight_ture <- t(apply(rank_ture,1,function(x){return(1/2^((x-1)/(5-1)))}))
  
  R_a <- sum(weight_pred*true_set)
  R_max <- sum(weight_ture*true_set)
  return(R_a/R_max*100)
}

## MAE (the smaller, the better)

MAE <- function(predict, test){
  diff <- abs(predict-test)
  MAE <- mean(diff[diff != Inf] , na.rm = T)
  return(MAE)
}

## ROC (the larger, the better)

ROC_4 <- function(predict, test){
  ## set filter = 4, which means the rating > 4 is good item, rating <=4 is bad item
  ## actual ratings are from 0-5, correspond to the data 1-6
  filter <- matrix(4, nrow = nrow(predict), ncol = ncol(predict))
  good_num <- mean((predict >= filter) == (test >= filter), na.rm = T)
  #n <- sum((predict >= filter)==1,na.rm=T)
  
  return(good_num)
}

### ADS Project 4 Spring 2018
### Fangbing Liu, Yanjun Lin
### Process Data

MStrain <- read.csv("../data/MS_sample/data_train.csv")
MStest  <- read.csv("../data/MS_sample/data_test.csv")
Movietrain <- read.csv("../data/eachmovie_sample/data_train.csv")
Movietest  <- read.csv("../data/eachmovie_sample/data_test.csv")

### For data MS
MS_data <- function(tr_data,test_data){
  user_num <- which(tr_data$V1 == "C")
  user_id <- tr_data$V2[user_num]
  Vroot_id <- unique(tr_data$V2[which(tr_data$V1 == "V")])
  MS_mat <- matrix(0, nrow = length(user_id), ncol = length(Vroot_id))
  rownames(MS_mat) <- as.character(user_id)
  colnames(MS_mat) <- as.character(Vroot_id[order(Vroot_id)])
  test_mat <- MS_mat
  
  for(i in 1:(nrow(tr_data)-1)){
    if(tr_data[i,2] == "C"){ 
      userid <- tr_data[i,3]
      while(tr_data[i+1,2] == "V"){
        MS_mat[as.character(userid), as.character(tr_data[i+1,3])] = 1
        if((i+1) != nrow(tr_data)){
          i <- i+1
        }
        else break
      }
    }
  }
  
  for(i in 1:(nrow(test_data)-1)){
    if(test_data[i,2] == "C"){ 
      userid <- test_data[i,3]
      while(test_data[i+1,2] == "V"){
        test_mat[as.character(userid), as.character(test_data[i+1,3])] = 1
        if((i+1) != nrow(test_data)){
          i <- i+1
        }
        else break
      }
    }
  }
  return(list(train=MS_mat,test=test_mat))
}

all_data <- MS_data(MStrain,MStest)
MS_train <- all_data$train
MS_test  <- all_data$test
save(MS_train, file = "../output/data/MS_train.RData")
save(MS_test, file = "../output/data/MS_test.RData")

### For dara eachmovie
Movie_data <- function(tr_data,test_data){
  movie_id <- sort(unique(tr_data$Movie))
  user_id <- sort(unique(tr_data$User))
  Movie_mat <- matrix(NA, nrow = length(user_id), ncol = length(movie_id))
  rownames(Movie_mat) <- user_id
  colnames(Movie_mat) <- movie_id
  test_mat <- Movie_mat
  
  for(i in 1:length(movie_id)){
    col.name <- movie_id[i]
    index <- which(tr_data$Movie == col.name)
    scores <- tr_data[index, 4] #Score 
    users <- tr_data[index, 3]  #User
    index2 <- which(user_id %in% users)
    Movie_mat[index2, i] = scores
    Movie_mat[!index2, i] = NA
  }
  
  for(i in 1:length(movie_id)){
    col.name <- movie_id[i]
    index <- which(test_data$Movie == col.name)
    scores <- test_data[index, 4] #Score 
    users <- test_data[index, 3]  #User
    index2 <- which(user_id %in% users)
    test_mat[index2, i] = scores
    test_mat[!index2, i] = NA
  }
  
  return(list(train=Movie_mat,test=test_mat))
}
all_data <- Movie_data(Movietrain,Movietest)
Movie_train <- all_data$train
Movie_test  <- all_data$test
save(Movie_train, file = "../output/data/Movie_train.RData")
save(Movie_test, file = "../output/data/Movie_test.RData")
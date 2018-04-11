### ADS Project 4 Spring 2018
### Fangbing Liu
### Process Data

MStrain <- read.csv("../data/MS_sample/data_train.csv")
MStest  <- read.csv("../data/MS_sample/data_test.csv")
Movietrain <- read.csv("../data/eachmovie_sample/data_train.csv")
Movietest  <- read.csv("../data/eachmovie_sample/data_test.csv")

### For data MS
MS_data <- function(data){
  user_num <- which(data$V1 == "C")
  user_id <- data$V2[user_num]
  Vroot_id <- unique(data$V2[which(data$V1 == "V")])
  MS_mat <- matrix(0, nrow = length(user_id), ncol = length(Vroot_id))
  rownames(MS_mat) <- as.character(user_id)
  colnames(MS_mat) <- as.character(Vroot_id[order(Vroot_id)])
  
  for(i in 1:(nrow(data)-1)){
    if(data[i,2] == "C"){ 
      userid <- data[i,3]
      while(data[i+1,2] == "V"){
        MS_mat[as.character(userid), as.character(data[i+1,3])] = 1
        if((i+1) != nrow(data)){
          i <- i+1
        }
        else break
      }
    }
  }
  return(MS_mat)
}
MS_train <- MS_data(MStrain)
MS_test  <- MS_data(MStest)
save(MS_train, file = "../output/data/MS_train.RData")
save(MS_test, file = "../output/data/MS_test.RData")

### For dara eachmovie
Movie_data <- function(data){
  movie_id <- sort(unique(data$Movie))
  user_id <- sort(unique(data$User))
  Movie_mat <- matrix(NA, nrow = length(user_id), ncol = length(movie_id))
  rownames(Movie_mat) <- user_id
  colnames(Movie_mat) <- movie_id
  
  for(i in 1:length(movie_id)){
    col.name <- movie_id[i]
    index <- which(data$Movie == col.name)
    scores <- data[index, 4] #Score 
    users <- data[index, 3]  #User
    index2 <- which(user_id %in% users)
    Movie_mat[index2, i] = scores
    Movie_mat[!index2, i] = NA
  }
  return(Movie_mat)
}
Movie_train <- Movie_data(Movietrain)
Movie_test  <- Movie_data(Movietest)
save(Movie_train, file = "../output/data/Movie_train.RData")
save(Movie_test, file = "../output/data/Movie_test.RData")
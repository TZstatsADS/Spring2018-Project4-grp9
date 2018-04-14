### ADS Project 4 Spring 2018
### Fangbing Liu
### Memory-based 
### Similarity Weights(MSD of movie data with intersection)

# n_intersect <- matrix(NA, nrow = nrow(Movie_train), ncol = nrow(Movie_train))
# for(i in 1:nrow(Movie_train)){
#   for(j in 1:nrow(Movie_train)){
#     r_i <- Movie_train[i,]
#     r_j <- Movie_train[j,]
#     n_intersect[i,j] <- length(intersect(which(!is.na(r_i)), which(!is.na(r_j))))
#   }
# }
# mean(n_intersect) # 45

MSD_intersect <- function(data){
  n <- nrow(data) # number of users
  dissim <- matrix(NA, n, n)
  rownames(dissim) <- rownames(data)
  colnames(dissim) <- rownames(data)
  
  for(i in 1:n){
    for(j in 1:n){
      r_i <- data[i,]
      r_j <- data[j,]
      index <- intersect(which(!is.na(r_i)), which(!is.na(r_j)))
      dissim[i,j] <- mean((r_i[index] - r_j[index])^2)
    }
  }
  sim <- round((max(dissim, na.rm = T) - dissim)/max(dissim, na.rm = T),4)
  return(sim)
}
weight_MSD_Movie_intersect <- MSD_intersect(Movie_train)
save(weight_MSD_Movie_intersect, file = "../output/Similarity_Weight/weight_MSD_Movie_intersect.RData")

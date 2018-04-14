### ADS Project 4 Spring 2018
### Fangbing Liu
### Memory-based 
### Similarity Weights(Pearson/MSD)

load("../output/data/MS_train.RData")
load("../output/data/Movie_train.RData")

### Pearson Correlation
weight_pearson_MS <- round(cor(t(MS_train), method = "pearson"),4)
save(weight_pearson_MS, file = "../output/Similarity_Weight/weight_pearson_MS.RData")

weight_pearson_Movie <- round(cor(t(Movie_train), method = "pearson", use = "pairwise.complete.obs"),4)
save(weight_pearson_Movie, file = "../output/Similarity_Weight/weight_pearson_Movie.RData")

### Mean Square Difference

MSD <- function(data){
  n <- nrow(data) # number of users
  dissim <- matrix(NA, n, n)
  rownames(dissim) <- rownames(data)
  colnames(dissim) <- rownames(data)
  
  for(i in 1:n){
    for(j in 1:n){
      r_i <- data[i,]
      r_j <- data[j,]
      dissim[i,j] <- mean((r_i - r_j)^2, na.rm = T)
      if(is.nan(dissim[i,j])){
        dissim[i,j] = NA
      }
    }
  }
  sim <- round((max(dissim, na.rm = T) - dissim)/max(dissim, na.rm = T),4)
  return(sim)
}

weight_MSD_MS <- MSD(MS_train)
save(weight_MSD_MS, file = "../output/Similarity_Weight/weight_MSD_MS.RData")

weight_MSD_Movie <- MSD(Movie_train)
save(weight_MSD_Movie, file = "../output/Similarity_Weight/weight_MSD_Movie.RData")


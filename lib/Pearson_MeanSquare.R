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
weight_pearson_Movie <- weight_pearson_Movie*mat
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

### Similarity Weights(MSD of movie data with intersection > 30)

n_intersect <- matrix(NA, nrow = nrow(Movie_train), ncol = nrow(Movie_train))
for(i in 1:nrow(Movie_train)){
  for(j in 1:nrow(Movie_train)){
    r_i <- Movie_train[i,]
    r_j <- Movie_train[j,]
    n_intersect[i,j] <- length(intersect(which(!is.na(r_i)), which(!is.na(r_j))))
  }
}
mean(n_intersect) # 45

mat <- ifelse(n_intersect < 30, 0, 1)

weight_MSD_Movie_30 <- mat*weight_MSD_Movie
save(weight_MSD_Movie_30, file = "../output/Similarity_Weight/weight_MSD_Movie_30.RData")


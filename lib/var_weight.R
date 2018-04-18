
#################################################################
#################################################################

## Variance Weighting
get_v_i <- function(mat=Movie_train){
  var_vector <- apply(mat, 2, var, na.rm=TRUE)
  var_vector[is.na(var_vector)] <- 0
  var_max <- max(var_vector, na.rm = TRUE)
  var_min <- min(var_vector, na.rm = TRUE)
  v_i <- (var_vector - var_min)/var_max
  return(v_i)
}

var_weight_assign <- function(i, j, v, mat=Movie_train){
  r_i <- scale(mat[i, ])
  r_j <- scale(mat[j, ])
  index <- intersect(which(!is.na(r_i)), which(!is.na(r_j)))
  weight <- sum(v[index]*r_i[index]*r_j[index])/sum(v[index])
  return(weight)
}

var_weight_matrix <- function(mat = Movie_train){
  n = dim(mat)[1]
  n_items <- ncol(mat)
  mat_weight = matrix(1, nrow=n, ncol=n)
  v <- get_v_i(mat = mat)
  #########################
  v <- v/sum(v)
  mat <- t(scale(t(mat)))
  mat[is.na(mat)] <- 0
  mat_v <- t(t(mat) * v)
  mat_weight <- mat_v %*% t(mat)
  V_correction <- matrix(NA, ncol=n,nrow=n)
  for ( i in 1:n){
    print(i)
    int_mask <- (mat[i,] * t(mat)) != 0
    V_correction[i,] <- colSums(int_mask * v)
  }
  mat_weight <- mat_weight/V_correction
  #########################
  return(mat_weight)
}

#################################################################
#################################################################
MSD_var <- function(data){
  n <- nrow(data) # number of users
  dissim <- matrix(0, n, n)
  rownames(dissim) <- rownames(data)
  colnames(dissim) <- rownames(data)
  v <- get_v_i(mat = data)
  for(i in 1:(n-1)){
    print(i)
    print(Sys.time())
    for(j in (i+1):n){
      r_i <- data[i,]
      r_j <- data[j,]
      #index <- intersect(which(!is.na(r_i)), which(!is.na(r_j)))
      dissim[i,j] <- weighted.mean((r_i - r_j)^2, w = v, na.rm = T)
      
      #weight <- sum(v[index]*dissim[i,j],na.rm = T)/sum(v[index])
      
      if(is.nan(dissim[i,j])){
        dissim[i,j] = NA
      }
      #dissim[j,i] <- dissim[i,j]
    }
  }
  dissim[lower.tri(dissim, diag = FALSE)] <- dissim[upper.tri(dissim, diag = FALSE)]
  sim <- round((max(dissim, na.rm = T) - dissim)/max(dissim, na.rm = T),4)
  return(sim)
}

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

## Apply it to training data
load("../output/data/Movie_train.RData")
load("../output/data/MS_train.RData")

var_weight_MS <- var_weight_matrix(mat = MS_train)
var_weight_MS[is.na(var_weight_MS)] = 0
var_weight_MS <- round(var_weight_MS,3)
save(var_weight_MS, file = "../output/variance_weight/var_weight_MS.RData")


var_weight_Movie <- var_weight_matrix(mat = Movie_train)
var_weight_Movie[is.na(var_weight_Movie)] = 0
var_weight_Movie <- round(variance_weight_Movie,3)
var_weight_Movie <- var_weight_Movie*mat
save(var_weight_Movie, file = "../output/variance_weight/var_weight_Movie.RData")

## Apply it to training data
load("../output/data/Movie_train.RData")
load("../output/data/MS_train.RData")

MSD_var_weight_MS <- MSD_var(data = MS_train)
MSD_var_weight_MS[is.na(MSD_var_weight_MS)] = 0
MSD_var_weight_MS <- round(MSD_var_weight_MS,4)
save(MSD_var_weight_MS, file = "../output/variance_weight/MSD_var_weight_MS.RData")

MSD_var_weight_Movie <- MSD_var(data = Movie_train)
MSD_var_weight_Movie[is.na(MSD_var_weight_Movie)] = 0
MSD_var_weight_Movie <- round(MSD_var_weight_Movie,4)
MSD_var_weight_Movie <- MSD_var_weight_Movie*mat
save(MSD_var_weight_Movie, file = "../output/variance_weight/MSD_var_weight_Movie.RData")



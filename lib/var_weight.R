
#################################################################
#################################################################

## Variance Weighting
get_v_i <- function(mat=Movie_train){
  var_vector <- apply(mat, 2, var, na.rm=TRUE)
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
  mat_weight = matrix(1, nrow=n, ncol=n)
  v <- get_v_i(mat = mat)
  for (i in 1:(n-1)){
    print(i)
    print(Sys.time())
    for (j in (i+1):n){
      weight <- var_weight_assign(i, j, v, mat = mat)
      mat_weight[i, j] <- weight
      mat_weight[j, i] <- weight
    }
  }
  return(mat_weight)
}

#################################################################
#################################################################

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
save(var_weight_Movie, file = "../output/variance_weight/var_weight_Movie.RData")

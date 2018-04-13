


#################################################################
#################################################################

## Selecting Neighborhoods - best N neighborhods
neighbors.select <- function(sim_weights, N = 50){
  ## Input: sim_weights: similarity weights matrix
  ##                  N: amount of best neighbors
  ## Output: A list of neighbors's index for each users
  top.neighbors = list()
  coverage = c()
  for(i in 1:nrow(sim_weights)){
    vec1 = sim_weights[i,]
    sort_abs = sort(abs(vec1),decreasing = T)
    index = which((abs(vec1) >= sort_abs[1+N]) & (vec1 != 1))
    top.neighbors[[i]] = index
    coverage = union(coverage,index)
  }
  coverage = round(length(coverage)/nrow(sim_weights),4)*100
  
  print(paste0("The coverage percent is ", coverage,"%"))
  return(top.neighbors)
}


## Make predictions - Average z-score

predict.MS <- function(test = MS_test, train = MS_train,
                       sim_weights, top.neighbors){
  prediction.matrix = matrix(0, ncol = ncol(train), nrow = nrow(train))
  avg.ratings = apply(train, 1, mean, na.rm = T)
  sd.ratings = apply(train, 1, sd, na.rm = T)
  test.col = colnames(test)
  test.row = rownames(test)
  ### Average z-score
  for(i in 1:nrow(train)){
    neighbor.weights = sim_weights[i,top.neighbors[[i]]]
    neighbor.ratings = train[top.neighbors[[i]], ]
    r_i = avg.ratings[i] 
    sd_i = sd.ratings[i] 
    weight_sum =sum(neighbor.weights, na.rm = T)
    each_neighbor = ((neighbor.ratings-avg.ratings[top.neighbors[[i]]])/sd.ratings[top.neighbors[[i]]]) * neighbor.weights
    prediction.matrix[i,] = r_i +  sd_i*apply(each_neighbor, 2, sum, na.rm=T) / weight_sum
  }
  colnames(prediction.matrix) = colnames(train)
  rownames(prediction.matrix) = rownames(train)
  prediction1 = prediction.matrix[test.row,test.col]
  return(prediction1)
}

## Prediction
predict.Movie <- function(test = Movie_test, train = Movie_train, 
                          sim_weights, top.neighbors){
  prediction.matrix = matrix(NA, ncol = ncol(test), nrow = nrow(test))
  test.loc = is.na(test)
  avg.ratings = apply(train, 1, mean, na.rm = T)
  sd.ratings = apply(train, 1, sd, na.rm = T)
  for(i in 1:nrow(train)){
    train.col = colnames(test)[which(!is.na(test[i,]))]
    neighbor.weights = sim_weights[i,top.neighbors[[i]]]
    neighbor.ratings = train[top.neighbors[[i]], train.col]
    ### Average z-score
    r_i = avg.ratings[i] 
    sd_i = sd.ratings[i] 
    weight_sum =sum(neighbor.weights, na.rm = T)
    each_neighbor = ((neighbor.ratings-avg.ratings[top.neighbors[[i]]])/sd.ratings[top.neighbors[[i]]]) * neighbor.weights
    prediction.matrix[i,!test.loc[i,]] = round((r_i +  sd_i* apply(each_neighbor, 2, sum, na.rm=T)/weight_sum),0)
    
  }
  return(prediction.matrix)
}

############################################################

### Example: Pearson + Varaiance Weighting + Best N neighbor + Average z-score predict

## Movie dataset
load("~/Documents/GitHub/Spring2018-Project4-grp9/output/Similarity_Weight/weight_pearson_Movie.RData")
load("~/Documents/GitHub/Spring2018-Project4-grp9/output/variance_weight/var_weight_Movie.RData")
load("~/Documents/GitHub/Spring2018-Project4-grp9/output/data/Movie_test.RData")
load("~/Documents/GitHub/Spring2018-Project4-grp9/output/data/Movie_train.RData")
pearson_var_Movie = var_weight_Movie * weight_pearson_Movie
pearson_var_Movie_neighbor =  neighbors.select(pearson_var_Movie, N=20)
pearson_var_Movie_pred = predict.Movie(sim_weights = pearson_var_Movie,top.neighbors = pearson_var_Movie_neighbor)

## MS dataset
load("~/Documents/GitHub/Spring2018-Project4-grp9/output/Similarity_Weight/weight_pearson_MS.RData")
load("~/Documents/GitHub/Spring2018-Project4-grp9/output/variance_weight/var_weight_MS.RData")
load("~/Documents/GitHub/Spring2018-Project4-grp9/output/data/MS_test.RData")
load("~/Documents/GitHub/Spring2018-Project4-grp9/output/data/MS_train.RData")
pearson_var_MS = var_weight_MS * weight_pearson_MS
pearson_var_MS_neighbor = neighbors.select(pearson_var_MS,N=20)
pearson_var_MS_pred = predict.MS(sim_weights =pearson_var_MS, top.neighbors = pearson_var_MS_neighbor)

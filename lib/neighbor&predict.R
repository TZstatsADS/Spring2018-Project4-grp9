


#################################################################
#################################################################

## Selecting Neighborhoods - best N neighborhods
neighbors.select <- function(sim_weights, N = 50){
  ## Input: sim_weights: similarity weights matrix
  ##                  N: amount of best neighbors
  ## Output: A list of neighbors's index for each users
  top.neighbors = list()
  coverage = 0
  for(i in 1:nrow(simweights)){
    vec1 = simweights[i,]
    index_ = which((abs(vec1) > thresholding) & (vec1 != 1))
    top.neighbors[[i]] = index_
    coverage = coverage + length(top.neighbors[[i]])
  }
  return(top.neighbors)
}

pearson.var.MS = var_weight_MS * pearson_weight_MS
simweights=pearson.var.MS
pearson.var.MS.pred = pred.matrix.ms(simweights =pearson.var.MS, top.neighbors =pearson.var.MS.neighbor)


pred.matrix.ms <- function(testdat = MS_test, traindat = MS_train, simweights, top.neighbors){
  #testdat = MS_test
  #traindat = MS_train
  #simweights = pearson_weight_MS
  #top.neighbors = results_MS
  prediction.matrix = matrix(0, ncol = ncol(traindat), nrow = nrow(traindat))
  avg.ratings = apply(traindat, 1, mean, na.rm = T)
  test.col = colnames(testdat)
  test.row = rownames(testdat)
  # neighbor.avg <- apply(traindat,1,mean,na.rm=T)
  # neighbor.weights = try[user,top.neighbors]
  for(i in 1:nrow(traindat)){
    #train.col = colnames(testdat)[which(!is.na(testdat[i,]))]
    #train.loc = traindat[i,train.col]
    neighbor.weights = simweights[i,top.neighbors[[i]]]
    neighbor.ratings = traindat[top.neighbors[[i]], ]
    # neighbor.avg = ifelse(length(top.neighbors[[i]]) <2, mean(traindat[top.neighbors[[i]], !test.loc[i,]]),apply(traindat[top.neighbors[[i]], ],1,mean,na.rm= T))
    if(length(top.neighbors[[i]]) <2){
      if(length(top.neighbors[[i]]) < 1){
        prediction.matrix[i,] = avg.ratings[i]
      }
      else{prediction.matrix[i,] = avg.ratings[i] + (neighbor.ratings-avg.ratings[top.neighbors[[i]]]) * neighbor.weights / sum(neighbor.weights, na.rm = T)}
    }
    else{
      prediction.matrix[i,] = avg.ratings[i] + apply((neighbor.ratings-avg.ratings[top.neighbors[[i]]]) * neighbor.weights, 2, sum, na.rm=T) / sum(neighbor.weights, na.rm = T)
    }
    
  }
  colnames(prediction.matrix) = colnames(traindat)
  rownames(prediction.matrix) = rownames(traindat)
  prediction1 = prediction.matrix[test.row,test.col]
  return(prediction1)
}

pearson.var.movie = mat_variance_weight_movie * pearson_weight
pearson.var.movie.neighbor =  neighbors.select(pearson.var.movie)
pearson.var.movie.pred = pred.matrix.movie(simweights =pearson.var.movie,top.neighbors = pearson.var.movie.neighbor)

pearson.var.MS = var_weight_MS * pearson_weight_MS
pearson.var.MS.neighbor = neighbors.select(pearson.var.MS)


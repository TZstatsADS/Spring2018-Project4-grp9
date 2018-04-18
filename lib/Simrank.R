# Regular simrank
simrank_regular <- function(dense_mat, maxIteration = 30, C1 = 0.8) {
  dense_mat[is.na(dense_mat)] <- 0
  n_users <- nrow(dense_mat)
  n_movies <- ncol(dense_mat)
  nodesnum <-  n_users + n_movies
  trans_mat <-  matrix(rep(0, nodesnum*nodesnum), nrow = nodesnum, ncol = nodesnum)
  trans_mat[1:n_users,(n_users+1):nodesnum] <-  dense_mat
  trans_mat[(n_users+1):nodesnum,1:n_users] <-  t(dense_mat)
  trans_mat <- sweep(trans_mat, 2, colSums(trans_mat), "/")
  sim_mat <-   diag(rep((1), nodesnum))
  
  for (i in 1:maxIteration) {
    sim_mat = C1 * (t(trans_mat) %*% sim_mat) %*% trans_mat + diag(rep((1-C1), nodesnum))
    sim_mat[sim_mat < 0.0001] <- 0
    if (i == maxIteration) {
      diag(sim_mat) <- rep(1, nodesnum)
      sim_mat_user <-  sim_mat[1:n_users,1:n_users]
      sim_mat_movie <- sim_mat[(n_users+1):nodesnum,(n_users+1):nodesnum]
    }
    print(i) 
  }  
  return(list(sim_mat, sim_mat_user, sim_mat_movie))
}


# N/P Simrank
simrank_adv <- function(dense_mat, maxIteration = 30, C1 = 0.8) {
  dense_mat[is.na(dense_mat)] <- 0
  n_users <- nrow(dense_mat)
  n_movies <- ncol(dense_mat)
  nodesnum <-  n_users + n_movies
  
  trans_mat_p <-  matrix(rep(0, nodesnum*nodesnum), nrow = nodesnum, ncol = nodesnum)
  dense_mat_p <- dense_mat - 3
  dense_mat_p[dense_mat_p<0] <- 0
  
  trans_mat_n <-  matrix(rep(0, nodesnum*nodesnum), nrow = nodesnum, ncol = nodesnum)
  dense_mat_n <- -(dense_mat - 4)
  dense_mat_n[dense_mat_n<0 | dense_mat_n == 4] <- 0
  
  trans_mat_p[1:n_users,(n_users+1):nodesnum] <-  dense_mat_p 
  trans_mat_p[(n_users+1):nodesnum,1:n_users] <-  t(dense_mat_p)
  trans_mat_n[1:n_users,(n_users+1):nodesnum] <-  dense_mat_n 
  trans_mat_n[(n_users+1):nodesnum,1:n_users] <- t(dense_mat_n)
  
  trans_mat_p <- sweep(trans_mat_p, 2, colSums(trans_mat_p), "/")
  trans_mat_p[is.na(trans_mat_p)] <- 0
  trans_mat_n <- sweep(trans_mat_n, 2, colSums(trans_mat_n), "/")
  trans_mat_n[is.na(trans_mat_n)] <- 0
  
  sim_mat_p <-  diag(rep((1), nodesnum))
  sim_mat_n <- diag(rep((1), nodesnum))
  
  for (i in 1:maxIteration) {
    sim_mat_p <- C1 * (t(trans_mat_p) %*% sim_mat_p) %*% trans_mat_p + diag(rep((1-C1), nodesnum))
    sim_mat_p[sim_mat_p <- 0.0001] = 0
    sim_mat_n <- C1 * (t(trans_mat_n) %*% sim_mat_n) %*% trans_mat_n + diag(rep((1-C1), nodesnum))
    sim_mat_n[sim_mat_n <- 0.0001] = 0
    
    if (i == maxIteration) {
      diag(sim_mat_p) <- rep(1, nodesnum)
      diag(sim_mat_n) <- rep(1, nodesnum)
      sim_mat_p_user <-  sim_mat_p[1:n_users,1:n_users]
      sim_mat_p_movie <- sim_mat_p[(n_users+1):nodesnum,(n_users+1):nodesnum]
      
      sim_mat_n_user <-  sim_mat_n[1:n_users,1:n_users]
      sim_mat_n_movie <- sim_mat_n[(n_users+1):nodesnum,(n_users+1):nodesnum]
    }
    print(i) 
  }  
  return(list(sim_mat_p, sim_mat_p_user, sim_mat_p_movie, sim_mat_n, sim_mat_n_user, sim_mat_n_movie))
}



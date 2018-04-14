library(plyr)
library(doSNOW)
library(mixtools)


#################
#     Train     #
#################
MultmixEM <- function(in_data, K=2, C=3, epsilon = 1e-07, maxit = 10000, verbose=FALSE, navie=TRUE){
  users <- nrow(in_data)
  items <- ncol(in_data)
  iter = 1
  gamma_array <- array(NA, dim = c(C,items,K))
  
  if(navie){
    rdirichlet <- function(a){
      y <- rgamma(length(a), a, 1)
      return(y / sum(y))
    }
    # initialize mu and gamma
    mu <- rep(1/C,C)
    for(i in 1:C){
      for(j in 1:items)
        gamma_array[i,j,] <- rdirichlet(rep(1,K))
    }
    pregamma <- gamma_array
    
    pi <- matrix(NA,nrow=users, ncol=C)
    
    delta_gamma <- Inf
    
    while(delta_gamma > epsilon){
      # Expectation
      idx1 <- rep(1:C, each=users*(items))
      idx2 <- rep((1:items), users*C)
      idx3 <- rep(c(t(in_data)),C)+1
      call <- cbind(idx1,idx2,idx3)
      prod_gamma <-  t(exp(colSums(log(array(gamma_array[call],dim = c(items,users,C)))))) * mu
      
      log_like <- sum(log(colSums(prod_gamma)))
      pi <- t(prod_gamma)/colSums(prod_gamma) # dim = users * clusters
      
      # Maximization
      mu <- colMeans(pi)
      for (c in 1:C){
        mask3d <- array(in_data,dim=c(users,items,K))==(rep(1:K,each=items*users)-1)
        gamma_array[c,,] <- colSums(mask3d * pi[,c])
        gamma_array[c,,] <- gamma_array[c,,]/colSums(pi)[c]
      }
      
      delta_gamma <- sqrt(mean((gamma_array-pregamma)^2))
      if(verbose){
        cat(iter,'/',maxit,', delta gamma: ',delta_gamma,', log likelihood: ',log_like,'\n',sep='')
      }
      
      pregamma<- gamma_array
      iter <- iter+1
      if(iter >= maxit){
        cat('Reach maximun iteration.')
        break
      }
    }
    return(list(gamma=gamma_array,mu=mu,log_likelihood = log_like))
  }else{# only work with MS data
    em.out <- multmixEM(MS_train,k=C,verb=verbose,epsilon = epsilon, maxit = maxit)
    mu <- em.out$lambda
    gamma_array[,,2] <- em.out$theta
    gamma_array[,,1] <- 1-em.out$theta
    return(list(gamma=gamma_array,mu=mu,log_likelihood = em.out$loglik))
  }
}


#################
#    Predict    #
#################
cluster_predict_MS <- function(in_data, gamma_array, mu){
  users <- nrow(in_data)
  items <- ncol(in_data)
  C <- dim(gamma_array)[1]
  K <- dim(gamma_array)[3]
  out <- matrix(NA,ncol=items,nrow=users)
 
  calculate <- function(i,users,items,C,gamma_array,in_data, mu){
    idx1 <- rep(1:C, each=users*(items-1))
    idx2 <- rep((1:items)[-i], users*C)
    temp <- in_data[,-i]+1
    idx3 <- rep(c(t(temp)),C)
    call <- cbind(idx1,idx2,idx3)
    prod_gamma <-  t(exp(colSums(log(array(gamma_array[call],dim = c(items-1,users,C)))))) * mu
    denominator <- colSums(prod_gamma)
    
    nominator <- colSums(prod_gamma*gamma_array[,i,2])
    
    return(nominator/denominator)
  }
  # parallel comput the result
  registerDoSNOW(makeCluster(4))
  out <- laply(1:items,calculate,
               users=users,
               items=items,
               C=C,
               gamma_array=gamma_array,
               in_data=in_data,
               mu=mu,
               .parallel = TRUE)
  
  return(t(out))
}

